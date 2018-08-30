{-# LANGUAGE TemplateHaskell #-}

module Run (run) where

import Import

import Web.Scotty
import Network.HTTP.Types.Status
import Network.Wai (remoteHost)
import Network.Socket.Internal (SockAddr (..))

import Database.Persist.Sql (toSqlKey)

import Model
import DTOs
import Worker


run :: RIO App ()
run = do
  mode <- asks appMode
  case mode of
    ModeServer ->
      server
    ModeWorker wops -> do
      app <- ask
      runRIO (WorkerApp app wops) worker
    _ ->
      logInfo "Not yet implemented"


server :: RIO App ()
server = do
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
      NewJobDTO job group <- jsonData
      job' <- db $ addJob job group
      json job'

    post "/work" $ do
      x <- remoteHost <$> request
      WorkRequestDTO name <- jsonData
      work <- case x of
        SockAddrInet _ host ->
          db $ getWork (Worker name host)
        _ -> raise "Needs to connect with an IP4 address"
      case work of
        Just w -> do
          status created201
          json w
        Nothing ->
          status noContent204

    post "/work/test" $ do
      status created201
      json (0 :: Int , "/some/path" :: String)

    put "/work/:id" $ do
      text "good"

    get "/groups" $ do
      x <- db getWorkGroups
      json x

    get "/groups/:id/jobs" $ do
      groupid <- toSqlKey <$> param "id"
      x <- db $ getJobsOfGroup groupid
      json x

    get "/workers" $ do
      json =<< db getWorkers

  where
    db = flip runReaderT () . liftRIO
