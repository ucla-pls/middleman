{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-
Module      : Middleman.Server.Model
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : BSD
Maintainer  : kalhuage@cs.ucla.edu
Stability   : experimental
Description : The model used on the server

-}
module Middleman.Server.Model
  ( inDB
  , migrateDB

  -- * Group
  , Group (..)
  , GroupId
  , createGroup
  , recursivelyDeleteGroup

  -- * JobDescription
  , JobDescription (..)
  , JobDescriptionId
  , createJobDescription
  , findJobDescription
  , jobDescriptionsWithGroup

  -- * Job
  , Job (..)
  , JobId
  , createJob

  -- * Worker
  , Worker (..)
  , WorkerId
  , upsertWorker

  -- * Work
  , Work (..)
  , WorkId
  , createWork
  , startWorkOnAvailableJob
  , findWorkDescription
  , finishWorkWithResult

  -- * Result
  , Result (..)
  , ResultId

  -- * Success
  , Success (..)

  -- * HasSqlPool
  , HasSqlPool (..)

  -- * Re-exports
  , Entity (..)
  )
  where

-- base?
import Data.Pool

-- rio
import RIO hiding ((^.), on)
import RIO.Time


-- persist
import qualified Database.Persist.Sql as P
import           Database.Persist.TH

-- esqueleto
import Database.Esqueleto

-- middleman
import           Middleman.Server.Exception
import           Middleman.Server.Model.Extra

share
  [ mkPersist sqlSettings
  , mkDeleteCascade sqlSettings
  , mkMigrate "migrateAll"]
  [persistLowerCase|
  Group json
    name Text
    timeout Double
    UniqueGroupName name
    deriving Show Generic

  JobDescription json
    derivation Text
    groupId GroupId
    UniqueJobDerivation derivation
    deriving Show Generic

  Job json
    descId JobDescriptionId
    workId WorkId Maybe
    output FilePath
    UniqueJobDescription descId
    UniqueJobWorkId workId !force
    deriving Show Generic

  Worker json
    hostname String
    ipaddress Word32
    UniqueWorkerId ipaddress
    deriving Show Generic

  Work json
    workerId WorkerId
    jobId JobId
    started UTCTime
    resultId ResultId Maybe
    UniqueResultId resultId !force
    deriving Show Generic

  Result json
    ended UTCTime
    success Success
    deriving Show Generic
|]

class HasSqlPool env where
  dbSqlPool :: Lens' env (Pool SqlBackend)

type DB a = forall m. (MonadIO m) => P.SqlPersistT m a

inDB ::
  (MonadUnliftIO m, MonadReader env m, HasSqlPool env)
  => ReaderT SqlBackend m a
  -> m a
inDB f = do
  p <- view dbSqlPool
  runSqlPool f p

migrateDB :: DB ()
migrateDB =
  runMigration migrateAll

createGroup ::
  Group -> DB (Entity Group)
createGroup grp = do
  P.insertUniqueEntity grp `orFail` ItemAlreadyExists grp

jobDescriptionsWithGroup ::
  GroupId -> DB [Entity JobDescription]
jobDescriptionsWithGroup groupId =
  select $ from $ \jobDesc -> do
     where_ (jobDesc ^. JobDescriptionGroupId ==. val groupId)
     return jobDesc

recursivelyDeleteGroup ::
  GroupId -> DB ()
recursivelyDeleteGroup groupId =
  deleteCascade groupId

createJobDescription ::
  JobDescription -> DB (Entity JobDescription)
createJobDescription jobDesc =
  P.insertUniqueEntity jobDesc `orFail` ItemAlreadyExists jobDesc

findJobDescription ::
  JobDescriptionId -> DB JobDescription
findJobDescription jobDId =
  P.get jobDId `orFail` ItemNotFoundException jobDId

createJob ::
  Job -> DB (Entity Job)
createJob job =
  P.insertUniqueEntity job `orFail` ItemAlreadyExists job

upsertWorker ::
  Worker -> DB (Entity Worker)
upsertWorker worker =
  P.upsert worker [ WorkerIpaddress P.=. workerIpaddress worker ]

createWork ::
  Work -> DB (Entity Work)
createWork work = do
  P.insertEntity work

startWorkOnAvailableJob ::
  UTCTime -> WorkerId -> DB (Maybe (Entity Work))
startWorkOnAvailableJob  workStarted workWorkerId = do
  result <- P.selectFirst [ JobWorkId P.==. Nothing ] []
  case result of
    Just (entityKey -> workJobId) -> do
      let workResultId = Nothing
      work <- createWork ( Work {..} )
      P.update workJobId [ JobWorkId P.=. Just (entityKey work) ]
      return ( Just work )
    Nothing -> do
      return Nothing

findWorkDescription ::
  WorkId -> DB (Entity Work, Entity JobDescription, Entity Group)
findWorkDescription workId = do
  lst <- select $ from $ \(
    work `InnerJoin` job `InnerJoin` jobd `InnerJoin` grp) -> do
    on (jobd ^. JobDescriptionGroupId ==. grp ^. GroupId)
    on (job ^. JobDescId ==. jobd ^. JobDescriptionId )
    on (work ^. WorkJobId ==. job ^. JobId )
    where_ ( work ^. WorkId ==. val workId)
    return (work, jobd, grp)
  case lst of
    (e:_) -> return e
    _ -> throwIO $ ItemNotFoundException workId

-- | Finishes Work with Result. Do not over-write the previous results.
finishWorkWithResult ::
  WorkId -> Result -> DB ()
finishWorkWithResult workId result= do
  resultId <- P.insert result
  P.updateWhere
    [ WorkId P.==. workId, WorkResultId P.==. Nothing ]
    [ WorkResultId P.=. Just resultId ]

  when (resultSuccess result /= Succeded) $ do
    P.updateWhere
      [ JobWorkId P.==. Just workId ]
      [ JobWorkId P.=. Nothing ]
