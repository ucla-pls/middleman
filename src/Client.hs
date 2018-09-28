{-# LANGUAGE TemplateHaskell #-}
module Client where

import Import

import DTOs
import Nix
import ServerAccess

import Data.Aeson.Lens
import Data.Aeson

import RIO.FilePath

data ClientOptions = ClientOptions
  { _copsHostUrl :: !String
  , _copsStoreUrl :: !String
  , _copsGroup :: !String
  , _copsDrvs :: ![FilePath]
  }

makeClassy ''ClientOptions

type ClientApp = OptionsWithApp ClientOptions

instance HasClientOptions ClientApp where
  clientOptions = lens extraOptions (\x y -> x { extraOptions = y })

instance HasServerAccess ClientApp where
  serverPort = Import.options . optionsPort
  serverName = clientOptions . copsHostUrl

client ::
  ClientOptions
  -> RIO App ()
client ops = ask >>= \app -> runRIO (OptionsWithApp app ops) $ do
  forM_ (ops ^.. copsDrvs . folded . to (\d -> (d, takeBaseName d))) $ \(drv, name) -> do

    r <- post "api/jobs" . toJSON $ NewJobDTO name (ops^.copsGroup)

    case (r ^. responseStatus . statusCode, r ^? responseBody . key "id" . _JSON) of
      (201, Just (i :: Int)) -> do
        logInfo $ "Created new job @ " <> displayShow i

        logInfo $ "Copying the derivation to the store"
        copyToStore (ops ^. copsStoreUrl) [drv]
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
