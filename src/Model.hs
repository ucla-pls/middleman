{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Model where

-- import           Control.Monad.IO.Class  (liftIO)

import           RIO
import           RIO.Time

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite
import           Database.Persist.TH

import           Conduit
import           Control.Monad.Logger (NoLoggingT)

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
    path String
    groupId WorkGroupId
    workId WorkId Maybe
    UniqueJobPath path
    deriving Show Generic

  Work json
    workerId WorkerId
    started UTCTime
    completed UTCTime Maybe
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

-- deriveJSON (def 3) ''Job
-- deriveJSON (def 4) ''Work
-- deriveJSON (def 6) ''Worker
-- deriveJSON (def 9) ''WorkGroup

-- class IsSqlBackend b => HasSqlBackend b r where
--   sqlBackendL :: Lens' r b

-- class IsSqlBackend b => HasSqlBackend b r where
--   sqlBackendL :: Lens' r b


runDB ::
  ReaderT SqlBackend (NoLoggingT (ResourceT (RIO r))) a
  -> RIO r a
runDB f = runSqlite "example.sqlite" $ f


type DB m a = ReaderT SqlBackend m a

migrateDB :: RIO r ()
migrateDB =
  runDB $ runMigration migrateAll

getJobs :: RIO r [Entity Job]
getJobs =
  runDB $ do
    selectList [] []

getUnworkedJobs :: RIO r [Entity Job]
getUnworkedJobs =
  runDB $ do
    selectList [ JobWorkId ==. Nothing ] []

getJobsOfGroup :: WorkGroupId -> RIO r [Entity Job]
getJobsOfGroup gid =
  runDB $ do
    selectList [ JobGroupId ==. gid ] []

getWorkGroups :: RIO r [Entity WorkGroup]
getWorkGroups =
  runDB $ do
    selectList [] []

startWork ::
  (MonadIO m)
  => UTCTime
  -> Worker
  -> DB m (Entity Work)
startWork t worker = do
  wid <- either entityKey id <$> insertBy worker
  insertEntity (Work wid t Nothing)

getWork :: Worker -> RIO r (Maybe (WorkId, FilePath))
getWork wrk = do
  t <- getCurrentTime
  runDB $ do
    x <- selectFirst [ JobWorkId ==. Nothing ] []
    case x of
      Just j -> do
        w <- startWork t wrk
        update (entityKey j) [ JobWorkId =. Just (entityKey w)]
        return $ Just (entityKey w, (jobPath $ entityVal j))
      Nothing ->
        return Nothing

addJob :: String -> String ->  RIO r (Entity Job)
addJob path group =
  runDB $ do
    g <- either entityKey id <$> insertBy (WorkGroup group)
    upsert (Job path g Nothing) [ JobGroupId =. g ]


getWorkers :: RIO r [Entity Worker]
getWorkers =
  runDB $ selectList [] []
