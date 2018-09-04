module Worker where

import Data.Aeson
import Data.Aeson.Lens
import Control.Lens

import Import
import Nix
import DTOs
import ServerAccess
import RIO.Directory

worker :: RIO WorkerApp ()
worker = do
  -- Ask for available jobs

  r <- post "work" . toJSON $ WorkRequestDTO "myname"

  logDebug $ displayShow r
  case r ^? responseBody . _JSON of
    Just (WorkNeededDTO path wid) -> do
      -- Do the work.
      logDebug $ "Got " <> displayShow path <> " from the server."
      succ <- realizeDrv path "here"
      logDebug $ "Returned "<> displayShow succ

      case succ of
        Just [fp] -> do

          path <- canonicalizePath fp

          store <- view wopsStoreUrl
          didCopy <- copyToStore store [path]
          when (not didCopy) $
            error $ "Could not transfer " ++ show path ++ " to server."

          logDebug "Transfer succeeded..."

          r' <- put ("work/" ++ show wid ++ "/path") (toJSON $ path)

          case r' ^. responseStatus . statusCode of
            200 ->
              logDebug "Successfully completed job"
            _ ->
              logError $ "Failed " <> displayShow (r' ^. responseStatus)

        _ -> do
          logError $ "Got invalid result from running derivation: " <> displayShow succ
          error "Got invalid results from running derivation"
      -- Push the work to the server.
    Nothing -> do
      logError "No more jobs"
      return ()

