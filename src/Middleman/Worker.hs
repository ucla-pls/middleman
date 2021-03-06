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

-- base
import qualified Data.Map as Map
import System.IO.Error (isDoesNotExistError)
import Text.Read

-- rio
import RIO.Process
import RIO.Directory
import RIO.List as List
import qualified RIO.Text as Text
import qualified RIO.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC

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
  , _wopsForever :: !Bool
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

  runForever <- view wopsForever

  if runForever
    then
      runPool ps $ \pool -> foreverWaitForServer $ do
        logDebug $ "Waiting for work..."
        waitForActivePool pool
        safePullWork workerId
          ( dispatch pool . runWork info )
    else
      safePullWork workerId (runWork info)

  where

    runWork info (WorkDetails {..}) = do
      let
        Entity ji jd = workDetailsJobDescription
        drv = jobDescriptionDerivation jd
        drvlink = "middleman-link/" ++ (show . keyToInt $ ji)
        timelimit = groupTimeout . entityVal $ workDetailsGroup

      logInfo $ "Got work from server " <> display workDetailsId
      logDebug $ "Running " <> display workDetailsId
      ( do
          result <- performJob timelimit drvlink drv
          case result of
            Right output -> do
              logInfo $ display workDetailsId
                <> " completed successfully."
              Nix.copyToStore (infoStoreUrl info) [output]
              Client.finishWork workDetailsId Succeded BL.empty
            Left Nothing -> do
              logError $ display workDetailsId
                <> " timed out after " <> display timelimit <> "."
              Client.finishWork workDetailsId Timeout BL.empty
            Left (Just msg) -> do
              logError $ display workDetailsId
                <> " failed."
              Client.finishWork workDetailsId Failed msg
        ) `withException` (
        \(e :: SomeException) -> do
          logError $ display workDetailsId
            <> " was rudely interrupted by " <> displayShow e <> ", retrying it."
          Client.finishWork workDetailsId Retry BL.empty
        )
      removeIfExists drvlink

    safePullWork workerId fm =
      bracketOnError (Client.pullWork workerId)
        ( maybe (return ())
          (\(WorkDetails {..}) -> do
              logError $ display workDetailsId <> " was rudely interrupted."
              Client.finishWork workDetailsId Retry BL.empty
          ))
        ( maybe waitForInput fm )

    foreverWaitForServer m = do
      forever $ catch m $ \(e :: HttpClientException) -> do
        logError (displayShow e)
        waitForInput

    waitForInput = do
      let delay = 60.0
      logDebug $ "Waiting for server for " <> display delay <> " seconds..."
      threadDelay (seconds delay)

-- * Access commands

-- | Perform a job with derivation name
performJob ::
  ( HasLogFunc env, HasProcessContext env)
  => Double
  -- ^ Timelimit in seconds
  -> FilePath
  -> Derivation
  -- ^ Derivation name
  -> RIO env (Either (Maybe BL.ByteString) OutputPath)
performJob timelimit drvlink drv = do
  succ <- timeout (seconds timelimit)
    $ Nix.realizeDerivation drv drvlink
  return $ case succ of
    Just (Right [fp]) -> Right fp
    Just (Left msg) -> Left (Just msg)
    Just (Right a) -> Left (Just $ "Does not contain a single line, but: " <> BLC.pack (show a))
    Nothing -> Left Nothing

-- * Regulator

mkRegulators ::
  (MonadReader env m, MonadIO m, HasLogFunc env)
  => Maybe (Double, Double)
  -> [Regulator m]
mkRegulators =
  maybe [] ((:[]) . outOfMemoryRegulator)
  where
    outOfMemoryRegulator (freeLower, freeUpper) = Regulator $ do
      liftIO ( tryAny $ readFileUtf8 "/proc/meminfo" ) >>= \case
        Right txt -> do
          let
            res = Map.fromList
              . map (over _2 (read . Text.unpack . Text.takeWhile numbers . Text.dropWhile (not . numbers)))
              . map (Text.span (/= ':'))
              $ Text.lines txt

            totalram = Map.findWithDefault (0 :: Double) "MemTotal" res
            availableram = Map.findWithDefault (0 :: Double) "MemAvailable" res
          let
            free = 100.0 * (availableram / totalram)
            comparation = compareRange free (freeLower, freeUpper)
          logDebug $ "Running memory regulator, found free "
            <> display free
            <> " is " <> displayShow comparation
            <> " in range " <> display freeLower <> " to " <> display freeUpper
          return comparation
        Left _ -> do
          logError "Cannot find sysInfo on system"
          return EQ

    compareRange item (lower, upper)
      | item < lower = LT
      | item > upper = GT
      | otherwise = EQ


    inText txt c = Text.any (== c) txt

    numbers = inText "0123456789"

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

-- from https://stackoverflow.com/questions/8502201/remove-file-if-it-exists
removeIfExists :: MonadIO m => FilePath -> m ()
removeIfExists fileName = liftIO $ removeFile fileName `catch` handleExists
  where
    handleExists e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e

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
