module Client where

import Import

import DTOs
import Nix
import ServerAccess

import Data.Aeson.Lens
import Data.Aeson

client :: RIO ClientApp ()
client = do
  drv <- view copsDrv
  mbs <- exportNar [drv]

  bs <- case mbs of
    Nothing -> do
      logError $ "Could not export NAR for " <> displayShow drv
      error "Could not export NAR"
    Just x -> return $ x

  r <- post "jobs" . toJSON $ NewJobDTO drv "group"

  case (r ^. responseStatus . statusCode, r ^? responseBody . key "id" . _JSON) of
    (201, Just (i :: Int)) -> do
      logInfo $ "Created new job @ " <> displayShow i
      r' <- put ("jobs/" ++ show i ++ "/nar") bs
      let s = r' ^. responseStatus
      case s ^. statusCode of
        201 ->
          logInfo $ "Successfully pushed derivation to the server"
        sc ->
          logError $ "Error uploading NAR to server, expected 201, got "
             <> display sc <> ": "
             <> displayBytesUtf8 (s ^. statusMessage)
    (x, a) ->
      logError $ "Got " <> displayShow a <> " from the server and " <> display x <> ": " <>
        displayBytesUtf8 (r^.responseStatus.statusMessage)
