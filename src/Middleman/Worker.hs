{-# LANGUAGE TemplateHaskell #-}
{-
Module      : Middleman.Worker
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : BSD
Maintainer  : kalhuage@cs.ucla.edu
Stability   : experimental
Description : The worker of the middleman.

-}
module Middleman.Worker
  where


-- rio
import RIO.Process
import RIO.List as List
import qualified RIO.Text as Text
import qualified RIO.ByteString.Lazy as BL


-- sysinfo
import           System.SysInfo

-- http-client
import Network.HTTP.Client

-- UnliftIO
import UnliftIO

-- middleman
import Import
import Control.Concurrent.Pool
import Middleman.Client as Client
import Middleman.DTO
import Network.HTTP.Client.Helper
import Nix.Tools as Nix
import Nix.Types as Nix

data WorkerOptions = WorkerOptions
  { _wopsServer :: !Server
  , _wopsMaxJobs :: !Int
  , _wopsFreeMemory :: !(Maybe (Double, Double))
  , _wopsRegulateTime :: !Rational
  }

makeClassy ''WorkerOptions

type WorkerApp = OptionsWithApp (WorkerOptions, Manager)

instance HasWorkerOptions WorkerApp where
  workerOptions = extraOptionsL . _1

instance HasServerAccess WorkerApp where
  serverL = workerOptions . wopsServer
  managerL = extraOptionsL . _2

worker :: WorkerOptions -> RIO App ()
worker ops = do
  manager <- liftIO $ newManager defaultManagerSettings
  ask >>= \app -> runRIO (OptionsWithApp app (ops, manager)) workerApp

workerApp :: RIO WorkerApp ()
workerApp = do
  info <- Client.getInfo

  Entity workerId w <- Client.ensureWorker =<< getHostName

  logInfo $ "Starting "
    <> display workerId <> " with name "
    <> display (workerName w)

  ps <- PoolSettings
    <$> view wopsMaxJobs
    <*> ( mkRegulators <$> view wopsFreeMemory )
    <*> view wopsRegulateTime
    <*> pure False

  runPool ps $ \pool -> foreverWaitForServer $ do
    logDebug $ "Waiting for work..."
    waitForActivePool pool
    safePullWork workerId $ \(WorkDetails {..})-> do
      let
        jd = entityVal workDetailsJobDescription
        drv = jobDescriptionDerivation jd
        timelimit = groupTimeout . entityVal $ workDetailsGroup

      logDebug $ "Got work from server " <> display workDetailsId
      dispatch pool $ do
        logDebug $ "Running " <> display workDetailsId
        ( do
            result <- performJob timelimit drv
            case result of
              Right output -> do
                logInfo $ display workDetailsId
                  <> " completed successfully."
                Nix.copyToStore (infoStoreUrl info) [output]
                Client.finishWork workDetailsId Succeded
              Left True -> do
                logError $ display workDetailsId
                  <> " timed out after " <> display timelimit <> "."
                Client.finishWork workDetailsId Timeout
              Left False -> do
                logError $ display workDetailsId
                  <> " failed."
                Client.finishWork workDetailsId Failed
          ) `withException` (
          \(e :: SomeException) -> do
            logError $ display workDetailsId
              <> " was rudely interrupted."
            logError $ displayShow e
            Client.finishWork workDetailsId Retry
          )
  where
    safePullWork workerId fm =
      bracketOnError (Client.pullWork workerId)
        ( maybe (return ())
          (\(WorkDetails {..}) -> do
              logError $ display workDetailsId <> " was rudely interrupted."
              Client.finishWork workDetailsId Retry
          ))
        ( maybe waitForInput fm )

    foreverWaitForServer m =
      forever . catch m $ \case
        HttpException e -> do
          logError (displayShow e)
          waitForInput
        e -> do
          throwIO e

    waitForInput = do
      let delay = 10.0
      logDebug $ "Waiting for server for " <> display delay <> " seconds..."
      threadDelay (seconds delay)

-- * Access commands

-- | Perform a job with derivation name
performJob ::
  ( HasLogFunc env, HasProcessContext env)
  => Double
  -- ^ Timelimit in seconds
  -> Derivation
  -- ^ Derivation name
  -> RIO env (Either Bool OutputPath)
performJob timelimit drv = do
  threadId <- myThreadId
  succ <- timeout (seconds timelimit)
    $ Nix.realizeDerivation drv
          ("middleman-link/" ++ show threadId)
  return $ case succ of
    Just (Just [fp]) -> Right fp
    Nothing -> Left True
    _ -> Left False

-- * Regulator

mkRegulators ::
  (MonadReader env m, MonadIO m, HasLogFunc env)
  => Maybe (Double, Double)
  -> [Regulator m]
mkRegulators =
  maybe [] ((:[]) . outOfMemoryRegulator)
  where
    outOfMemoryRegulator (freeLower, freeUpper) = Regulator $ do
      liftIO sysInfo >>= \case
        Right sys -> do
          let
            free = 100.0 *
              (fromIntegral $ freeram sys) / (fromIntegral $ totalram sys)
            comparation = compareRange free (freeLower, freeUpper)
          logDebug $ "Running memory regulator, found free "
            <> display free
            <> " is " <> displayShow comparation
            <> " in range " <> display freeLower <> "to" <> display freeUpper
          return comparation
        Left _ -> do
          logError "Cannot find sysInfo on system"
          return EQ

    compareRange item (lower, upper)
      | item < lower = LT
      | item > upper = GT
      | otherwise = EQ



-- * Exceptions

data WorkerException
  = JobExecutedWrong String
  | CouldNotGetHostName String
  deriving (Typeable)

instance Show WorkerException where
  show = \case
    JobExecutedWrong str ->
      "Job executed wrong:" ++ show str
    CouldNotGetHostName str ->
      "Could not get hostname: " ++ show str

instance Exception WorkerException

-- * Utils

-- | Gets the hostname in the current system using this command
getHostName ::
  (HasProcessContext env, HasLogFunc env, MonadUnliftIO m, MonadReader env m)
  => m Text.Text
getHostName = do
  s <- proc "hostname" [] readProcessStdout_
    `catchAny` (throwIO . CouldNotGetHostName . show)
  case Text.lines <$> Text.decodeUtf8' (BL.toStrict s) of
    Right (hostname:_) -> return hostname
    _ -> throwIO (CouldNotGetHostName "Could not parse output")

-- | Converts any double to an integer
seconds :: Double -> Int
seconds a =
  round (a * 1e6)
