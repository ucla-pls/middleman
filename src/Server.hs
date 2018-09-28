{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Server (server) where

import           Import                    hiding ((<.>))

import           Database.Persist.Sql      (Entity, SqlBackend, toSqlKey)
import qualified Database.Persist.Sql      as DB
import           Database.Persist.Sqlite   (withSqlitePool)
import           System.Posix.Files        (createSymbolicLink, readSymbolicLink)
import           RIO.Directory
import           RIO.FilePath              (takeFileName, (<.>), (</>))
import           RIO.Process
import qualified RIO.Text.Lazy             as TL
import qualified RIO.Text                  as Text
import qualified RIO.ByteString.Lazy            as BL

import           Control.Monad.Trans.Except
import           Network.HTTP.Types.Status
import           Network.Socket.Internal   (SockAddr (..))
import           Network.Wai
import           Web.Scotty.Trans
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import           Control.Monad.Logger      (runNoLoggingT)
import           Data.Pool
import qualified Data.ByteString.Builder as BL

import qualified Dhall

import Data.Success

import           DTOs
import           Model
import           Nix
import           WebServer

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

instance HasLogFunc ServerApp where
  logFuncL = app . logFuncL

instance HasSqlPool ServerApp where
  dbSqlPool = dbPool

instance HasProcessContext ServerApp where
  processContextL = app . processContextL

instance HasLocalStore ServerApp where
  localStore = serverOptions . sopsLocalStore

server ::
  ServerOptions
  -> RIO App ()
server ops = ask >>= \a ->
  runNoLoggingT
  . withSqlitePool (ops ^. sopsConnectionString) 10
  $ \pool -> runRIO (ServerApp a ops pool) $ do
    b <- view sopsRunMigration
    when b $ migrateDB

    port <- view optionsPort
    logInfo $ "Starting server at " <> display port
    env <- ask
    scottyT port (runRIO env) serverDescription

-- | Creates a new job on the server
createNewJob ::
  (HasSqlPool env, HasLogFunc env, HasLocalStore env)
  => String
  -> String
  -> RIO env (Entity Job)
createNewJob drv group = do
  logDebug $ "Create new work at " <> displayShow drv <> " with group " <> displayShow group
  e <- makeJob drv group
  storeCreateGCRoot drv (drv <.> "drv")
  return $ e

-- | Publish a job
publishJob ::
  (HasSqlPool env, HasLogFunc env, HasLocalStore env, HasProcessContext env)
  => JobId
  -> RIO env (Either TL.Text (Entity Job))
publishJob key = runExceptT $ do
  logDebug $ "Publishing work " <> displayShow key
  jm <- lift . runDB $ DB.get key
  case jm of
    Just job -> do
      let
        drv = jobDerivation job
        path = "/nix/store" </> drv <.> "drv"
      moutput <- readDerivationOutput path
      case takeFileName <$> moutput of
        Just output -> lift $ do
          storeCreateGCRoot (drv ++ "-output") output
          let job' = job { jobOutput = Just output }
          runDB $ DB.repsert key job'
          return $ DB.Entity key job'
        Nothing -> do
          fail $ "Could not read derivation: " ++ show path
    Nothing ->
      fail $ "Key not found: " ++ show key

-- | Create new work and return the job
findNewWork ::
  (HasSqlPool env, HasLogFunc env)
  => Worker
  -> RIO env (Maybe (Entity Job))
findNewWork worker = do
  logDebug $ "Find work with worker " <> displayShow worker
  getSomeJobWithNewWork worker

-- | Mark the work as done
finishWork ::
  (HasSqlPool env, HasLogFunc env)
  => WorkId
  -> Success
  -> RIO env (Either TL.Text (Entity Work))
finishWork wid succ = runExceptT $ do

  logDebug $ "Finish work " <> displayShow wid <> " with status " <> displayShow succ

  w <- maybe (throwE "Could not find work.") return
    =<< (lift . runDB $ DB.get wid)

  j <- maybe (throwE "Work does not point to a valid job") return
    =<< (lift . runDB $ DB.get (workJobId w))

  output <- maybe (throwE "Job does not have a valid output path.") return $
    jobOutput j

  when (succ == Succeded) $ do
    found <- doesPathExist $ "/nix/store" </> output
    when (not found) $
      throwE . utf8BuilderToLazyText $
        "Did not find " <> displayShow (jobOutput j) <> " in the store."

  work <- lift $ markWorkAsCompleted wid succ

  maybe (throwE "Unexpected error") return $ work


storeCreateGCRoot ::
  (HasLogFunc env, HasLocalStore env, MonadUnliftIO m, MonadReader env m)
  => String
  -> String
  -> m ()
storeCreateGCRoot name storepath = do
  gcroot <- view storeGCRoot
  let
    srcf = (gcroot </> name)
    dest = ("/nix/store" </> storepath)
  logDebug $ "Creating GCRoot " <> displayShow srcf <> " -> " <> displayShow dest <> "."
  catchIO (liftIO $ createSymbolicLink dest srcf) $ \e -> do
    logDebug "Could not create symbolic link:"
    logDebug $ displayShow e
    f <- catchIO (liftIO $ Just <$> readSymbolicLink srcf) . const $ return Nothing
    logDebug $ "The symbolic link points to " <> displayShow f
    when (f /= Just dest) $ do
      logDebug $ "So we delete it a try again"
      liftIO $ do
        removeFile srcf
        createSymbolicLink dest srcf

serverDescription ::
  (HasSqlPool env, HasLogFunc env, HasProcessContext env, HasLocalStore env)
  => ScottyT TL.Text (RIO env) ()
serverDescription = do
  api
  webserver


-- | The api of the server
api ::
  (HasSqlPool env, HasLogFunc env, HasProcessContext env, HasLocalStore env)
  => ScottyT TL.Text (RIO env) ()
api = do
    get "/api/jobs" $ do
      unworked <- param "unworked" `rescue` (const $ pure False)
      x <- lift $
        if unworked
        then getUnworkedJobs
        else getJobs
      json x

    post "/api/jobs" $ do
      NewJobDTO path group <- jsonData
      job' <- lift $ createNewJob path group
      status created201
      json job'

    put "/api/jobs/:id/publish" $ do
      key <- toSqlKey <$> param "id"
      ejob <- lift $ publishJob key
      case ejob of
        Left msg -> do
          status badRequest400
          text msg
        Right job ->
          json job

    post "/api/work" $ do
      req <- request
      WorkRequestDTO name <- jsonData
      case remoteHost req of
        SockAddrInet _ host -> do
          mjob <- lift $ findNewWork (Worker name host)
          case mjob of
            Just (DB.Entity _ job) -> do
              let mdto = do
                   let wndtoDerivation = jobDerivation job
                   wndtoOutput <- jobOutput job
                   wndtoWorkId <- DB.fromSqlKey <$> jobWorkId job
                   return $ WorkNeededDTO {..}
              case mdto of
                Just dto -> do
                  status created201
                  json dto
                Nothing -> do
                  fail $ "Something went wrong"
            Nothing ->
              status noContent204
        _ -> do
          status $ badRequest400 { statusMessage = "Needs to connect with an IPv6 address"}

    put "/api/work/:id/success" $ do
      key <- toSqlKey <$> param "id"
      WorkSuccededDTO succ <- jsonData
      lift (finishWork key succ) >>= \case
        Left msg -> do
          status badRequest400
          text msg
        Right job ->
          json job
