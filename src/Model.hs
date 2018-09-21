{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Model where

-- import           Control.Monad.IO.Class  (liftIO)

import           RIO
import           RIO.Time

import           Data.Pool
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.TH

import           Database.Persist.MoreTypes ()
import           Data.Success (Success(..))

share [ mkPersist
          sqlSettings {
            mpsEntityJSON =
              Just EntityJSON
              { entityToJSON = 'entityIdToJSON
              , entityFromJSON = 'entityIdFromJSON
              }
            }
      , mkMigrate "migrateAll"] [persistLowerCase|
  Job json
    derivation String
    groupId WorkGroupId
    workId WorkId Maybe
    output String Maybe
    UniqueJobPath derivation
    UniqueJobWorkId workId !force
    deriving Show Generic

  Work json
    workerId WorkerId
    jobId JobId
    started UTCTime
    completed UTCTime Maybe
    success Success Maybe
    deriving Show Generic

  Worker json
    hostname String
    ipaddress Word32
    UniqueWorkerId ipaddress
    deriving Show Generic

  WorkGroup json
    name String
    GroupName name
    deriving Show Generic
|]

class HasSqlPool env where
  dbSqlPool :: Lens' env (Pool SqlBackend)

runDB ::
  (MonadReader env m, MonadUnliftIO m, HasSqlPool env)
  => ReaderT SqlBackend m a
  -> m a
runDB f = do
  p <- view dbSqlPool
  runSqlPool f p

migrateDB ::
  (HasSqlPool env)
  => RIO env ()
migrateDB =
  runDB $ runMigration migrateAll

getJobs ::
  HasSqlPool env
  => RIO env [Entity Job]
getJobs =
  runDB $ do
    selectList [] []

getUnworkedJobs ::
  HasSqlPool env
  => RIO env [Entity Job]
getUnworkedJobs =
  runDB $ do
    selectList [ JobWorkId ==. Nothing, JobOutput !=. Nothing ] []

getJobsOfGroup ::
  HasSqlPool env
  => WorkGroupId
  -> RIO env [Entity Job]
getJobsOfGroup gid =
  runDB $ do
    selectList [ JobGroupId ==. gid ] []

getWorkGroups ::
  HasSqlPool env
  => RIO env [Entity WorkGroup]
getWorkGroups =
  runDB $ do
    selectList [] []

startWork ::
  (MonadIO m, IsSqlBackend env, PersistUniqueWrite env)
  => UTCTime
  -> JobId
  -> Worker
  -> ReaderT env m (Entity Work)
startWork t jid worker = do
  wid <- either entityKey id <$> insertBy worker
  insertEntity (Work wid jid t Nothing Nothing)

markWorkAsCompleted ::
  HasSqlPool env
  => WorkId
  -> Success
  -> RIO env (Maybe (Entity Work))
markWorkAsCompleted wid succ = do
  t <- getCurrentTime
  runDB $ do
    update wid [ WorkSuccess =. Just succ, WorkCompleted =. Just t ]
    when (succ == Retry) $  do
      getBy (UniqueJobWorkId $ Just wid) >>= \case
        Just x ->
          update (entityKey x) [ JobWorkId =. Nothing]
        Nothing ->
          return ()
    getEntity wid

getSomeJobWithNewWork ::
  (HasSqlPool env)
  => Worker
  -> RIO env (Maybe (Entity Job))
getSomeJobWithNewWork wrk = do
  t <- getCurrentTime
  runDB $ do
    selectFirst [ JobWorkId ==. Nothing, JobOutput !=. Nothing ] [] >>= \case
      Just j -> do
        w <- startWork t (entityKey j) wrk
        j' <- updateGet (entityKey j) [ JobWorkId =. Just (entityKey w)]
        return $ Just (j { entityVal = j'})
      Nothing ->
        return Nothing

-- completeWork ::
--   (HasSqlPool env)
--   => WorkId
--   -> FilePath
--   -> RIO env ()
-- completeWork wid path = do
--   t <- getCurrentTime
--   runDB $ do
--     r <- updateGet wid [ WorkCompleted =. Just t ]
--     when (workPath r /= path) $
--       error $ "expected path to be " ++ path ++ " was " ++ workPath r

makeJob ::
  HasSqlPool env
  => String
  -> String
  -> RIO env (Entity Job)
makeJob path group =
  runDB $ do
    g <- either entityKey id <$> insertBy (WorkGroup group)
    upsertBy (UniqueJobPath path) (Job path g Nothing Nothing)
      [ JobGroupId =. g, JobOutput =. Nothing, JobWorkId =. Nothing  ]

-- setInStore ::
--   HasSqlPool env
--   => JobId
--   -> String
--   -> RIO env ()
-- setInStore key path =
--   runDB $ do
--     r <- updateGet key [ JobInStore =. True ]
--     when (jobPath r /= path) $
--       error $ "expected path to be " ++ path ++ " was " ++ jobPath r

getWorkers ::
  HasSqlPool env
  => RIO env [Entity Worker]
getWorkers =
  runDB $ selectList [] []
