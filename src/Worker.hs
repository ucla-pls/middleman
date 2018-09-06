module Worker where

import Data.Aeson
import Data.Aeson.Lens

import Database.Persist.Sql (Entity(..), fromSqlKey)

import Import hiding ((<.>))
import Nix
import DTOs
import Model
import ServerAccess
import RIO.Directory
import RIO.FilePath


worker :: WorkerOptions -> RIO App ()
worker ops = ask >>= \app -> runRIO (OptionsWithApp app ops) $ do
  -- Ask for available jobs
  let localworkername = "here"
  r <- post "api/work" . toJSON $ WorkRequestDTO (ops ^. wopsName)
  case r ^? responseBody . _JSON of
    Just (Entity _ job) -> do
      -- Do the work.

      let drv = "/nix/store" </> jobDerivation job <.> "drv"
      logDebug $ "Got " <> displayShow drv <> " from the server."

      (output, wid) <- case (do out <- jobOutput job; wid <- jobWorkId job; return (out,wid)) of
         Just (out, wid) ->
           return (out, wid)
         Nothing ->
           throwString "Badly formed data from the server"

      succ <- realizeDrv drv localworkername
      logDebug $ "Returned "<> displayShow succ

      case succ of
        Just [fp] -> do

          path <- canonicalizePath fp

          when (path /= "/nix/store" </> output) .
            throwString $ "Returned path different from the job output: " ++ show path ++ " /= " ++ show output

          copyToStore (ops ^. wopsStoreUrl) [path]
          logDebug "Transfer succeeded..."

          r' <- put ("api/work/" ++ show (fromSqlKey wid) ++ "/success") (toJSON $ WorkSuccededDTO True)

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

