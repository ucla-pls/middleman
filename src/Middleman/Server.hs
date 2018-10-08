{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-
Module      : Middleman.Server
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : BSD
Maintainer  : kalhuage@cs.ucla.edu
Stability   : experimental
Description : The middleman server
-}
module Middleman.Server (server, ServerOptions(..)) where

-- base
import           Data.Pool

-- containers
import qualified Data.Map as Map

-- rio
import RIO.Process
import RIO.Time
import RIO.List as List hiding (delete)
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Text.Lazy as TL
import qualified RIO.Text as Text

-- http-types
import           Network.HTTP.Types.Status

import           Network.Socket.Internal   (SockAddr (..))

-- wai
import           Network.Wai

-- wai-extra
import qualified Network.Wai.Middleware.RequestLogger

-- aseon
import Data.Aeson (ToJSON)

-- scotty
import           Web.Scotty.Trans

-- monad-logger
import           Control.Monad.Logger      (runNoLoggingT)

-- persist-postgresql
import           Database.Persist.Postgresql   (withPostgresqlPool, SqlBackend)


-- middleman
import           Import                    hiding ((<.>))
-- import           WebServer
import Nix.Tools (HasGCRoot(..))
import Nix.Types (Store)
import Middleman.Server.Control
import Middleman.WebServer
import Middleman.Server.Exception
import Middleman.DTO as DTO
import Middleman.Server.Model (HasSqlPool(..), migrateDB)
import qualified Middleman.Server.Model as DB

-- * ServerOptions

data ServerOptions = ServerOptions
  { _sopsConnectionString :: !Text
  , _sopsStoreUrl :: !Store
  , _sopsGCRoot :: !FilePath
  , _sopsTemplates :: !FilePath
  }
makeClassy ''ServerOptions

data ServerApp = ServerApp
  { _app :: App
  , _sOptions :: ServerOptions
  , _dbPool :: Pool SqlBackend
  }
makeLenses ''ServerApp

instance HasOptions ServerApp where
  options = app . Import.options

instance HasTemplates ServerApp where
  templatesL = serverOptions . sopsTemplates

instance HasServerOptions ServerApp where
  serverOptions = sOptions

instance HasGCRoot ServerApp where
  gcRootL = serverOptions . sopsGCRoot

instance HasLogFunc ServerApp where
  logFuncL = app . logFuncL

instance HasSqlPool ServerApp where
  dbSqlPool = dbPool

instance HasProcessContext ServerApp where
  processContextL = app . processContextL

-- * Server implementation

server ::
  ServerOptions
  -> RIO App ()
server ops = ask >>= \a ->
  runNoLoggingT
  . withPostgresqlPool (Text.encodeUtf8 $ ops ^. sopsConnectionString) 10
  $ \pool -> runRIO (ServerApp a ops pool) $ do
    m <- migrateDB
    if (not $ null m)
      then do
        mapM_ (BL.putStrLn . BL.fromStrict . Text.encodeUtf8) m
      else do
        port <- view optionsPort
        logInfo $ "Starting server at " <> display port
        env <- ask
        scottyT port (runRIO env) serverDescription

serverDescription :: API
serverDescription = do
  middleware (Network.Wai.Middleware.RequestLogger.logStdout)
  api
  webserver

type API = ScottyT TL.Text (RIO ServerApp) ()

-- | The api of the server
api :: API
api = do
  get "/api" $ do
    storeurl <- lift . view $ serverOptions . sopsStoreUrl
    json $ Info
      { infoStoreUrl = storeurl
      }
  groupPaths
  jobDescriptionPaths
  jobPaths
  workersPaths
  workPaths

groupPaths :: API
groupPaths = do
  get "/api/groups" $ do
    withName <- maybeParam "name"
    grps <- lift $ listGroups (DB.GroupQuery withName)
    json grps

  post "/api/groups/" $ do
    grp <- jsonData
    result <- lift . try $ createGroup grp
    case result of
      Left (ItemAlreadyExists r) -> do
        status badRequest400
        text ("Group already exist " <> tShow r)
        finish
      Right entity ->

        json entity
      Left e -> raise (tShow e)

  get "/api/groups/:id" $ do
    key <- param "id"
    findOrFail $ findGroup key

  delete "/api/groups/:groupId" $ do
    groupId <- param "groupId"
    lift $ deleteGroup groupId
    status ok200

  post "/api/groups/:groupId/retry" $ do
    groupId <- param "groupId"
    _ <- lift $ retryOldGroup groupId
    status ok200

jobDescriptionPaths :: API
jobDescriptionPaths = do
  post "/api/job-descriptions/" $ do
    descs :: [JobDescription] <- jsonData `rescue` const next
    results <- forM descs $ \desc ->
      fmap snd . lift $ submitJobDescription desc
    json results

  post "/api/job-descriptions/" $ do
    desc <- jsonData
    (new, result) <- lift $ submitJobDescription desc
    if new
      then do
      status created201
      json result
      else
      status forbidden403

  get "/api/job-descriptions/" $ do
    json =<< lift ( listJobDescriptions DB.JobDescriptionQuery )

  get "/api/job-descriptions/:id" $ do
    jobDescId <- param "id"
    findOrFail ( findJobDescription jobDescId )

  get "/api/job-descriptions/:id/job" $ do
    jobDescId <- param "id"
    findOrFail ( List.headMaybe <$> listJobs (DB.JobQuery (Just jobDescId)))

  post "/api/job-descriptions/:id/publish" $ do
    jobDescId <- param "id"
    result <- lift ( try $ publishJob jobDescId )
    case result of
      Left (DatabaseException e@(ItemNotFoundException _)) -> do
        status notFound404
        text (TL.pack . show $ e)
      Left (DatabaseException e@(ItemAlreadyExists _)) -> do
        status badRequest400
        text (TL.pack . show $ e)
      Left (NixException e) -> do
        status badRequest400
        text (TL.pack . show $ e)
      Left e -> do
        raise (TL.pack . show $ e)
      Right job -> do
        status created201
        json job

jobPaths :: API
jobPaths = do
  get "/api/jobs/" $ do
    desc <- maybeParam "desc"
    json =<< lift ( listJobs (DB.JobQuery desc) )

workersPaths :: API
workersPaths = do
  get "/api/workers" $ do
    withName <- maybeParam "name"
    json =<< lift ( listWorkers (DB.WorkerQuery withName))

  post "/api/workers/" $ do
    name <- newWorkerName <$> jsonData
    req <- request
    case remoteHost req of
      SockAddrInet _ ipaddress -> do
        result <- lift ( upsertWorker $ Worker name ipaddress )
        json result
      _ -> do
        status badRequest400
        text "Needs to connect with an IPv6 address"

  post "/api/workers/:workerId/work" $ do
    workerId <- param "workerId"
    result <- lift ( startWork workerId )
    case result of
      Just (Entity workId _) -> do
        lift (findWorkDetails workId) >>= \case
          Nothing -> raise "Something went wrong"
          Just wd -> json wd
      Nothing ->
        status status204

workPaths :: API
workPaths = do
  get "/api/work/" $ do
    json =<< lift ( listWork mempty)

  get "/api/work/details" $ do
    json =<< lift ( listWorkDetails mempty)

  get "/api/work/per-hour" $ do
    dts <- lift (listWorkDetails mempty)
    now <- liftIO $ getCurrentTime
    let grps =
          Map.fromListWith (+)
          . List.map (\WorkDetails{..} ->
                        case workDetailsResult of
                          Just (DB.Result {..}) | resultSuccess == DB.Succeded ->
                            (round ((resultEnded `diffUTCTime` now) / 3600), 1)
                          _ -> (0 :: Int , 0 :: Int)
                     )
          $ dts
    groups <- lift $ listGroupDetails (DB.GroupQuery Nothing)
    let js = DB.sumJobSummary $ Import.map (DB.groupDJobSummary) groups
    json $ (js, grps)

  get "/api/work/:workId" $ do
    workId <- param "workId"
    findOrFail (findWorkDetails workId)

  post "/api/work/:workId/:succ" $ do
    workId <- param "workId"
    succ <- param "succ"
    result <- lift . try $ finishWork workId succ
    case result of
      Left (DatabaseException e@(ItemNotFoundException _)) -> do
        status notFound404
        text (TL.pack . show $ e)
      Left (DatabaseException e@(ItemAlreadyExists _)) -> do
        status badRequest400
        text (TL.pack . show $ e)
      Left (InvalidInput e) -> do
        status badRequest400
        text (TL.pack . show $ e)
      Left (NixException e) -> do
        status badRequest400
        text (TL.pack . show $ e)
      Left e -> do
        raise (TL.pack . show $ e)
      Right job -> do
        status ok200
        json job


-- * Utils

findOrFail ::
  (ToJSON a, Monad m, ScottyError e)
  => m (Maybe a)
  -> ActionT e m ()
findOrFail m =
  lift m >>= \case
    Just r -> json r
    Nothing -> status notFound404

maybeParam ::
  (Parsable a, Monad m, ScottyError e)
  => TL.Text
  -> ActionT e m (Maybe a)
maybeParam s =
  (Just <$> param s) `rescue` const (return Nothing)

tShow :: Show a => a -> TL.Text
tShow = TL.pack . show
