module Client where

import Import

import DTOs
import Nix
import ServerAccess

import Data.Aeson.Lens
import Data.Aeson

import RIO.FilePath


client ::
  ClientOptions
  -> RIO App ()
client ops = ask >>= \app -> runRIO (OptionsWithApp app ops) $ do
  let drv = ops ^. copsDrv . to (takeBaseName)

  r <- post "api/jobs" . toJSON $ NewJobDTO drv "group"

  case (r ^. responseStatus . statusCode, r ^? responseBody . key "id" . _JSON) of
    (201, Just (i :: Int)) -> do
      logInfo $ "Created new job @ " <> displayShow i

      logInfo $ "Copying the derivation to the store"
      copyToStore (ops ^. copsStoreUrl) [ops ^. copsDrv]
      logInfo $ "Done"

      r' <- put ("api/jobs/" ++ show i ++ "/publish") (toJSON True)
      let s = r' ^. responseStatus
      case s ^. statusCode of
        200 ->
          logInfo $ "Successfully published derivation"
        sc ->
          logError $ "Error publishing the derivation expected 200, got: "
             <> display sc <> ": "
             <> displayBytesUtf8 (s ^. statusMessage)

    (x, a) ->
      logError $ "Got " <> displayShow a <> " from the server and " <> display x <> ": " <>
        displayBytesUtf8 (r^.responseStatus.statusMessage)
