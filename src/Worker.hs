{-# LANGUAGE TemplateHaskell #-}
module Worker where

import Data.Either
import Data.Maybe
import qualified Data.List as List

import Data.Aeson
import Data.Success


import RIO.Directory
import RIO.FilePath
import RIO.Process

import Data.ByteString.Lazy.Char8 as BS

-- sysinfo
import System.SysInfo

-- middleman
import Control.Concurrent.Pool
import Import hiding ((<.>))
import Nix
import DTOs
import ServerAccess

data WorkerOptions = WorkerOptions
  { _wopsServerUrl :: !String
  , _wopsStoreUrl :: !String
  , _wopsMaxJobs :: !Int
  , _wopsFreeMemory :: !(Maybe (Int, Int))
  }

makeClassy ''WorkerOptions

type WorkerApp = OptionsWithApp WorkerOptions

instance HasWorkerOptions WorkerApp where
  workerOptions = lens extraOptions (\x y -> x { extraOptions = y })

instance HasServerAccess WorkerApp where
  serverPort = Import.options . optionsPort
  serverName = workerOptions . wopsServerUrl

worker :: WorkerOptions -> RIO App ()
worker ops = ask >>= \app -> runRIO (OptionsWithApp app ops) $ do
  hostname <- getHostName

  maxJobs <- view wopsMaxJobs
  memory <- view wopsFreeMemory

  let
    regulators = case memory of
      Just (freeFrom, freeTo) ->
        let outOfMemoryRegulator = Regulator $ do
              sys <- liftIO $ sysInfo
              case fromIntegral . freeram <$> sys of
                  Right free -> do
                    let x = if (free > freeTo) then LT else if (free < freeFrom ) then GT else EQ
                    logDebug $ "Running memory regulator, found free "
                      <> displayShow free
                      <> " is " <> displayShow x
                      <> " in range " <> displayShow (freeFrom, freeTo)
                    return x
                  Left _ -> do
                    logError "Cannot find sys"
                    return EQ
         in [ outOfMemoryRegulator ]
      Nothing -> []

  runPool (PoolSettings maxJobs regulators 5.0) $ \pool -> forever $ do
    bracketOnError
      ( getWork hostname )
      reportFailureToServer
      ( maybe waitForInput ( computeAndCopyToServer pool ) )

  where
    reportFailureToServer = \case
      Just (_, _, workId) ->
        reportResult workId Retry
      Nothing ->
        return ()

    computeAndCopyToServer pool (drv, output, workId) = do
      dispatch pool $ do
        (flip onException) (reportResult workId Retry) $ do
          res <- performJob drv output
          case res of
            Just path -> do
              copyToStore (ops ^. wopsStoreUrl) [path]
              reportResult workId Succeded
            Nothing ->
              reportResult workId Failed

    waitForInput = do
      let delay :: Double = 1.0
      logDebug $ "No more work.. waiting for "
        <> display delay
        <> " seconds."
      threadDelay (round (delay * 1e6))

-- | Gets the hostname in the current system using this command
getHostName ::
  (HasProcessContext env, HasLogFunc env)
  => RIO env String
getHostName = do
  s <- proc "hostname" [] $
    readProcessStdout_
  return $ List.head (Import.lines $ BS.unpack s)

-- | Perform a job with derivation name
performJob ::
  ( HasLogFunc env, HasProcessContext env)
  => Derivation
  -- ^ Derivation name
  -> String
  -- ^ Expected output path
  -> RIO env (Maybe FilePath)
performJob drv output = do
  succ <- realizeDrv drv "here"
  case succ of
    Just fps ->
      case fps of
        [fp] -> do
          path <- canonicalizePath fp
          when (path /= "/nix/store" </> output)
            ( throwIO $ JobExecutedWrong
              ( "Returned path different: " ++ show path ++ " /= " ++ show output )
            )
          return $ Just path
        _ -> do
          throwIO . JobExecutedWrong $
            "Returned more than one path: " ++ show fps
    Nothing ->
      return Nothing

-- | getWork pools the server for work. If the server has work, return the Job
-- to work on, else return Nothing.
getWork ::
  (HasServerAccess env, MonadUnliftIO m, MonadReader env m)
  => String
  -> m (Maybe (Derivation, String, Int64))
getWork workerName = do
  r <-
    post "api/work"
      ( toJSON
        ( WorkRequestDTO
          { wrdtoHostName = workerName
          }
        )) `catch` ( throwIO . FailedRequest )

  case r ^. responseStatus . statusCode of
    204 ->
      return Nothing
    201 ->
      either
        ( throwIO . BadResponseFormat . (++ (" got " ++ show (r ^. responseBody))))
        ( pure . Just )
        ( parseResponseBody r )
    _ ->
      throwIO ( BadResponseFormat $ "Unexpected status: " ++ ( show $ r ^. responseStatus ))

  where
    parseResponseBody r =  do
      wnd <- eitherDecode (r ^. responseBody)
      return ( Derivation (wndtoDerivation wnd), wndtoOutput wnd, wndtoWorkId wnd )

-- | Report the result to the server
reportResult ::
  (HasServerAccess env, MonadUnliftIO m, MonadReader env m)
  => Int64
  -> Success
  -> m ()
reportResult wid val = do
  _ <-
    put ("api/work/" ++ show wid ++ "/success")
      ( toJSON $ WorkSuccededDTO val )
    `catch`
    (throwIO . FailedRequest)
  return ()

-- * Exceptions

data WorkerException
  = FailedRequest HttpException
  | BadResponseFormat String
  | JobExecutedWrong String
  deriving (Typeable)

instance Show WorkerException where
  show = \case
    FailedRequest e ->
      "Failed request: " ++ show e
    BadResponseFormat str ->
      "Received bad formatted response from server: " ++ show str
    JobExecutedWrong str ->
      "Job executed wrong:" ++ show str

instance Exception WorkerException
