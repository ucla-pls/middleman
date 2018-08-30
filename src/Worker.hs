module Worker where

import Import hiding (view)

import RIO.Process
import DTOs
import Network.Wreq

import Data.Aeson.Lens
import Control.Lens

worker :: RIO WorkerApp ()
worker = do
  -- Ask for available jobs
  port <- view optionsPort
  host <- view wopsHostUrl

  r <- liftIO $ post
    (host ++ ":" ++ show port ++ "/work/test")
    ((WorkRequestDTO "myname" ^. re _JSON) :: ByteString)

  logInfo $ displayShow r
  case r ^? responseBody . _JSON of
    Just (wid, path) -> do
      -- Do the work.
      logInfo $ "Got this from the server " <> displayShow ((wid, path) :: (Int, String))

      (rc, str) <- proc "echo" ["hello", "world"] readProcessStdout
      logInfo $ "Returned "<> displayShow str

      -- Push the work to the server.
    Nothing -> do
      logError "No more jobs"
      return ()
