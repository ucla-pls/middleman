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
  , GroupQuery (..)
  , Group (..)
  , GroupId
  , createGroup
  , listGroups
  , findGroup
  , recursivelyDeleteGroup

  -- * JobDescription
  , JobDescriptionQuery (..)
  , JobDescription (..)
  , JobDescriptionId
  , upsertJobDescription
  , findJobDescription
  , jobDescriptionsWithGroup
  , listJobDescriptions

  -- * Job
  , Job (..)
  , JobId
  , createJob
  , JobQuery (..)
  , listJobs
  , JobSummary (..)
  , jobSummary
  , retryOldJobs

  -- * Worker
  , WorkerQuery (..)
  , Worker (..)
  , WorkerId
  , listWorkers
  , upsertWorker

  -- * Work
  , WorkQuery (..)
  , Work (..)
  , WorkId
  , createWork
  , startWorkOnAvailableJob
  , finishWorkWithResult
  , listWork

  , WorkDetails (..)
  , findWorkDetails

  -- * Result
  , Result (..)
  , ResultId

  -- * Success
  , Success (..)

  -- * HasSqlPool
  , HasSqlPool (..)

  -- * Re-exports
  , module P
  , Derivation (..)
  )
  where

-- base?
import Data.Pool
import qualified Data.Map as Map

-- rio
import RIO hiding ((^.), on, set)
import RIO.Time

-- persist
import qualified Database.Persist.Sql as P
import           Database.Persist.TH

-- esqueleto
import Database.Esqueleto

-- aeson
import Data.Aeson.TH

-- middleman
import           Middleman.Server.Exception
import           Nix.Types
import           Middleman.Server.Model.Extra
import TH

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
    derivation Derivation
    groupId GroupId
    UniqueJobDerivation derivation
    deriving Show Generic Eq

  Job json
    descId JobDescriptionId
    workId WorkId Maybe
    output OutputPath
    UniqueJobDescription descId
    UniqueJobWorkId workId !force
    deriving Show Generic

  Worker json
    name Text
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

migrateDB ::
  (MonadUnliftIO m, MonadReader env m, HasSqlPool env)
  => m [Text]
migrateDB =
  inDB ( getMigration migrateAll )

createGroup ::
  Group -> DB (Entity Group)
createGroup grp = do
  P.insertUniqueEntity grp `orFail` ItemAlreadyExists grp

data GroupQuery = GroupQuery
  { hasName :: Maybe Text
  } deriving (Show, Eq)

listGroups ::
  GroupQuery -> DB [Entity Group]
listGroups (GroupQuery{..}) = do
  let query = maybe [] (\name -> [ GroupName P.==. name ]) hasName
  P.selectList query [ P.Asc GroupName ]

findGroup ::
  GroupId -> DB (Maybe (Entity Group))
findGroup groupId = do
  P.getEntity groupId

jobDescriptionsWithGroup ::
  GroupId -> DB [Entity JobDescription]
jobDescriptionsWithGroup groupId =
  select $ from $ \jobDesc -> do
     where_ (jobDesc ^. JobDescriptionGroupId ==. val groupId)
     return jobDesc

recursivelyDeleteGroup ::
  GroupId -> DB ()
recursivelyDeleteGroup groupId =
  P.delete groupId

-- | try to insert the job description into the database, if it already exist
-- set the flag to null.
upsertJobDescription ::
  JobDescription -> DB (Bool, Entity JobDescription)
upsertJobDescription jobDesc =
  (\e -> (entityVal e == jobDesc , e)) <$> P.upsert jobDesc []

findJobDescription ::
  JobDescriptionId -> DB (Maybe (Entity JobDescription))
findJobDescription jobDId =
  P.getEntity jobDId

data JobDescriptionQuery = JobDescriptionQuery
  {} deriving (Show, Eq)

listJobDescriptions ::
  JobDescriptionQuery
  -> DB [ Entity JobDescription ]
listJobDescriptions _ =
  P.selectList [] []

createJob ::
  Job -> DB (Entity Job)
createJob job =
  P.insertUniqueEntity job `orFail` ItemAlreadyExists job

data JobQuery = JobQuery
 { hasJobDescriptionId :: Maybe JobDescriptionId
 } deriving (Show, Eq)

listJobs ::
  JobQuery -> DB [Entity Job]
listJobs ( JobQuery {..} ) = do
  let query =
        maybe [] (\jd -> [ JobDescId P.==. jd]) hasJobDescriptionId
  P.selectList query []

retryOldJobs ::
  GroupId -> UTCTime -> DB Int
retryOldJobs gid before =
  fmap fromIntegral $ rawExecuteCount
    "UPDATE job SET work_id =DEFAULT FROM job as j INNER JOIN work as w ON j.work_id = w.id INNER JOIN job_description as jd ON j.desc_id = jd.id WHERE (jd.group_id = ? AND j.id = job.id AND w.started < ? AND w.result_id IS NULL)"
    [ PersistInt64 (fromSqlKey gid), PersistUTCTime before ]

data JobSummary = JobSummary
  { jobSActive :: Int
  , jobSSuccess :: Int
  , jobSTimeout :: Int
  , jobSFailed :: Int
  , jobSWaiting :: Int
  } deriving (Show, Eq)

jobSummary ::
  JobQuery
  -> DB JobSummary
jobSummary _ = do
  jobs <- select $ from $ \((job `LeftOuterJoin` work) `LeftOuterJoin` result) -> do
    on ( work ?. WorkResultId ==. just (result ?. ResultId) )
    on ( job ^. JobWorkId ==. work ?. WorkId )
    groupBy ( job ^. JobWorkId ==. work ?. WorkId, result ?. ResultSuccess )
    return ( just (job ^. JobWorkId ==. work ?. WorkId)
           , result ?. ResultSuccess
           , countRows)

  let sucCount =
        foldMap (\(Value run, Value succ, Value c) ->
                   Map.singleton (maybe False (const True) run, succ) c)
        jobs

  return $ JobSummary
    { jobSActive  = Map.findWithDefault 0 (True, Nothing) sucCount
    , jobSSuccess = Map.findWithDefault 0 (True, Just Succeded) sucCount
    , jobSTimeout  = Map.findWithDefault 0 (True, Just Timeout) sucCount
    , jobSFailed = Map.findWithDefault 0 (True, Just Failed) sucCount
    , jobSWaiting = Map.findWithDefault 0 (False, Nothing) sucCount
    }


data WorkerQuery = WorkerQuery
  { hasWorkerName :: Maybe Text
  } deriving (Show, Eq)

listWorkers ::
  WorkerQuery -> DB [Entity Worker]
listWorkers (WorkerQuery {..}) = do
  let query =
        maybe [] (\name -> [ WorkerName P.==. name ]) hasWorkerName
  P.selectList query [ P.Asc WorkerName ]

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

data WorkDetails =
  WorkDetails
  { workDetailsId :: !WorkId
  , workDetailsStarted :: !UTCTime
  , workDetailsWorkerId :: !WorkerId
  , workDetailsJobDescription :: !(Entity JobDescription)
  , workDetailsGroup :: !(Entity Group)
  , workDetailsResult :: !(Maybe Result)
  } deriving (Show, Generic)

deriveJSON (def 11) ''WorkDetails

findWorkDetails ::
  WorkId -> DB ( Maybe WorkDetails )
findWorkDetails workId = do
  lst <- select $ from $ \(
    work `InnerJoin` job `InnerJoin` jobd `InnerJoin` grp) -> do
    on (jobd ^. JobDescriptionGroupId ==. grp ^. GroupId)
    on (job ^. JobDescId ==. jobd ^. JobDescriptionId )
    on (work ^. WorkJobId ==. job ^. JobId )
    where_ ( work ^. WorkId ==. val workId)
    return (work, jobd, grp)

  case lst of
    ((entityVal -> work, jobd, grp):_) -> do
      result <- case workResultId work of
        Just rid -> P.get rid
        Nothing -> return $ Nothing
      return . Just $ WorkDetails
        { workDetailsId = workId
        , workDetailsStarted = workStarted work
        , workDetailsWorkerId = workWorkerId work
        , workDetailsJobDescription = jobd
        , workDetailsGroup = grp
        , workDetailsResult = result
        }
    _ -> return Nothing

-- | Finishes Work with Result. Do not over-write the previous results.
finishWorkWithResult ::
  WorkId -> Result -> DB ()
finishWorkWithResult workId result= do
  work <- P.get workId `orFail` ItemNotFoundException workId

  when (workResultId work /= Nothing) $ do
    throwIO $ ItemAlreadyExists work

  resultId <- P.insert result

  P.updateWhere
    [ WorkId P.==. workId, WorkResultId P.==. Nothing ]
    [ WorkResultId P.=. Just resultId ]

  when (resultSuccess result == Retry) $ do
    P.updateWhere
      [ JobWorkId P.==. Just workId ]
      [ JobWorkId P.=. Nothing ]

data WorkQuery = WorkQuery
  deriving (Show, Eq)

listWork ::
  WorkQuery -> DB [Entity Work]
listWork (WorkQuery) = do
  P.selectList [] [ P.Asc WorkStarted ]

-- * Displays for easy handeling

instance Display GroupId where
  display i = "Group(" <> display (fromSqlKey i) <> ")"

instance Display JobId where
  display i = "Job(" <> display (fromSqlKey i) <> ")"

instance Display JobDescriptionId where
  display i = "JobDescriptionId(" <> display (fromSqlKey i) <> ")"

instance Display WorkerId where
  display i = "Worker(" <> display (fromSqlKey i) <> ")"

instance Display WorkId where
  display i = "Work(" <> display (fromSqlKey i) <> ")"


instance Display GroupQuery where
  display (GroupQuery {..}) =
    fold $
    [ "Group Query("
    , foldMap (\name -> "has name = " <> display name) hasName
    , ")"
    ]
instance Display JobDescriptionQuery where
  display (JobDescriptionQuery) =
    fold $
    [ "JobDescriptionQuery("
    , ")"
    ]

instance Display JobQuery where
  display (JobQuery{..}) =
    fold $
    [ "JobDescriptionQuery("
    , foldMap (\dscr -> "has job desc. = " <> display dscr)
        hasJobDescriptionId
    , ")"
    ]

instance Display WorkQuery where
  display (WorkQuery) =
    fold $
    [ "WorkQuery("
    , ")"
    ]

instance Display WorkerQuery where
  display (WorkerQuery {..}) =
    fold $
    [ "WorkerQuery("
    , foldMap (\name -> "has name = " <> display name)
        hasWorkerName
    , ")"
    ]
