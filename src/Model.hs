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
    inStore Bool
    UniqueJobPath path
    deriving Show Generic

  Work json
    workerId WorkerId
    path String
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
    selectList [ JobWorkId ==. Nothing, JobInStore ==. True ] []

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
  -> FilePath
  -> Worker
  -> DB m (Entity Work)
startWork t path worker = do
  wid <- either entityKey id <$> insertBy worker
  insertEntity (Work wid path t Nothing)

getWork :: Worker -> (FilePath -> RIO r FilePath) -> RIO r (Maybe (WorkId, FilePath))
getWork wrk getWorkPath = do
  t <- getCurrentTime
  runDB $ do
    x <- selectFirst [ JobWorkId ==. Nothing ] []
    case x of
      Just j -> do
        p <- lift . liftRIO $ getWorkPath (jobPath (entityVal j))
        w <- startWork t p wrk
        update (entityKey j) [ JobWorkId =. Just (entityKey w)]
        return $ Just (entityKey w, (jobPath $ entityVal j))
      Nothing ->
        return Nothing

completeWork :: WorkId -> FilePath -> RIO r ()
completeWork wid path = do
  t <- getCurrentTime
  runDB $ do
    r <- updateGet wid [ WorkCompleted =. Just t ]
    when (workPath r /= path) $
      error $ "expected path to be " ++ path ++ " was " ++ workPath r

addJob :: String -> String ->  RIO r (Entity Job)
addJob path group =
  runDB $ do
    g <- either entityKey id <$> insertBy (WorkGroup group)
    upsert (Job path g Nothing False) [ JobGroupId =. g, JobInStore =. False, JobWorkId =. Nothing  ]

setInStore :: JobId -> String -> RIO r ()
setInStore key path =
  runDB $ do
    r <- updateGet key [ JobInStore =. True ]
    when (jobPath r /= path) $
      error $ "expected path to be " ++ path ++ " was " ++ jobPath r

getWorkers :: RIO r [Entity Worker]
getWorkers =
  runDB $ selectList [] []
