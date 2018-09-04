{-# LANGUAGE TemplateHaskell #-}

module Run (run) where

import Import

import Web.Scotty
import Network.HTTP.Types.Status
import Network.Wai (remoteHost)
import Network.Socket.Internal (SockAddr (..))

import Database.Persist.Sql (toSqlKey, fromSqlKey)
import RIO.Directory

import Model
import Nix
import DTOs
import Worker
import Client

run :: RIO App ()
run = do
  mode <- asks appMode
  case mode of
    ModeServer ->
      server
    ModeWorker wops -> do
      app <- ask
      runRIO (WorkerApp app wops) worker
    ModeClient cops -> do
      app <- ask
      runRIO (ClientApp app cops) client

server :: RIO App ()
server = do
  env <- ask
  let
    db :: (MonadIO m) => RIO App a -> m a
    db = runRIO env
  logInfo "Starting server"
  migrateDB
  liftIO . scotty 3000 $ do
    get "/jobs" $ do
      unworked <- param "unworked" `rescue` (const $ pure False)
      x <- db $
        if unworked
        then getUnworkedJobs
        else getJobs
      json x

    post "/jobs" $ do
      NewJobDTO path group <- jsonData
      job' <- db $ addJob path group
      status created201
      json job'

    put "/jobs/:id/nar" $ do
      key <- toSqlKey <$> param "id"
      bs <- body
      x <- runRIO env (importNar bs)
      case x of
        Just [fp] -> do
          db $ setInStore key fp
          status created201
        _ -> do
          error $ "Bad NAR file, returned " ++ show x

    post "/work" $ do
      x <- remoteHost <$> request
      WorkRequestDTO name <- jsonData
      work <- case x of
        SockAddrInet _ host ->
          db $ getWork (Worker name host)
             (\p -> do
                 drvOutput <- readDerivationOutput p
                 case drvOutput of
                   Just path -> return path
                   Nothing ->
                     error $ "Could not read output path from " ++ p
             )
        _ -> raise "Needs to connect with an IP4 address"
      case work of
        Just (wid, drv) -> do
          status created201
          json (WorkNeededDTO drv (fromIntegral $ fromSqlKey wid))
        Nothing ->
          status noContent204

    put "/work/:id/path" $ do
      wid <- toSqlKey <$> param "id"
      path <- jsonData
      b <- liftIO $ doesPathExist path
      if b
        then do
          db $ completeWork wid path
          status ok200
        else
          status badRequest400


    get "/groups" $ do
      x <- db getWorkGroups
      json x

    get "/groups/:id/jobs" $ do
      groupid <- toSqlKey <$> param "id"
      x <- db $ getJobsOfGroup groupid
      json x

    get "/workers" $ do
      json =<< db getWorkers

