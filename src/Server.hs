{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Server (server, ServerOptions(..)) where

-- wai?
import           Network.HTTP.Types.Status
import           Network.Socket.Internal   (SockAddr (..))
import           Network.Wai

-- aseon
import Data.Aeson (ToJSON)

-- scotty
import           Web.Scotty.Trans


  -- base
import           Data.Pool

-- monad-logger
import           Control.Monad.Logger      (runNoLoggingT)

-- persist-postgresql
import           Database.Persist.Postgresql   (withPostgresqlPool, SqlBackend)

-- rio
import RIO.Process
import qualified RIO.Text.Lazy as TL
import qualified RIO.Text as Text

-- middleman
import           Import                    hiding ((<.>))
import           WebServer

import Nix.Tools (HasGCRoot(..))

import Middleman.Server.Control
import Middleman.Server.Exception
import Middleman.DTO as DTO
import Middleman.Server.Model (HasSqlPool(..), migrateDB, Entity(..))

-- * ServerOptions

data ServerOptions = ServerOptions
  { _sopsRunMigration :: !Bool
  , _sopsConnectionString :: !Text
  , _sopsGCRoot :: !FilePath
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
    b <- view sopsRunMigration
    when b migrateDB

    port <- view optionsPort
    logInfo $ "Starting server at " <> display port
    env <- ask
    scottyT port (runRIO env) serverDescription

serverDescription ::
  (HasSqlPool env, HasGCRoot env, HasLogFunc env, HasProcessContext env )
  => ScottyT TL.Text (RIO env) ()
serverDescription = do
  api
  -- webserver


type API env =
 (HasSqlPool env, HasGCRoot env, HasLogFunc env, HasProcessContext env)
  => ScottyT TL.Text (RIO env) ()

-- | The api of the server
api :: API env
api = do
  get "/api" $ do
    json $ Info
      { infoStoreAddress = "localhost"
      }

  groupPaths
  jobDescriptionPaths
  jobPaths
  workerPaths
  workPaths


groupPaths :: API env
groupPaths = do
  get "/api/groups/" $ do
    grps <- lift $ listGroups
    json grps

  post "/api/groups/" $ do
    group <- jsonData
    result <- lift . try $ createGroup group
    case result of
      Left (ItemAlreadyExists grp) -> do
        status badRequest400
        text ("Group already exist " <> tShow grp)
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

jobDescriptionPaths :: API env
jobDescriptionPaths = do
  post "/api/job-descriptions/" $ do
    desc <- jsonData
    result <- lift . try $ submitJobDescription desc
    case result of
      Left (ItemAlreadyExists grp) -> do
        status badRequest400
        text ("Job description already exist: " <> tShow grp)
        finish
      Right entity ->
        json entity
      Left e -> raise (tShow e)

  get "/api/job-descriptions/:id" $ do
    jobDescId <- param "id"
    findOrFail ( findJobDescription jobDescId )

  post "/api/job-descriptions/:id/publish" $ do
    jobDescId <- param "id"
    json =<< lift ( publishJob jobDescId )


jobPaths :: API env
jobPaths = do
  get "/api/jobs/" $ do
    json =<< lift ( listJobs )

workerPaths :: API env
workerPaths = do
  post "/api/worker/" $ do
    name <- jsonData
    req <- request
    case remoteHost req of
      SockAddrInet _ ipaddress -> do
        json =<< lift ( upsertWorker $ Worker name ipaddress )
      _ -> do
        status badRequest400
        text "Needs to connect with an IPv6 address"

  post "/api/worker/:workerId/work" $ do
    workerId <- param "workerId"
    result <- lift ( startWork workerId )
    case result of
      Just (Entity workId _) -> do
        lift (findWorkDetails workId) >>= \case
          Nothing -> raise "Something went wrong"
          Just wd -> json wd
      Nothing ->
        status status204


workPaths :: API env
workPaths = do
  get "/api/work/" $ do
    json =<< lift ( listWork )

  get "/api/work/:workId" $ do
    workId <- param "workId"
    findOrFail (findWorkDetails workId)

  post "/api/work/:workId/:succ" $ do
    workId <- param "workId"
    succ <- param "succ"
    lift (finishWork workId succ)



-- * Utils

findOrFail ::
  (ToJSON a, Monad m, ScottyError e)
  => m (Maybe a)
  -> ActionT e m ()
findOrFail m =
  lift m >>= \case
    Just r -> json r
    Nothing -> status notFound404

tShow :: Show a => a -> TL.Text
tShow = TL.pack . show
