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
  , increaseTimeoutOfGroup
  , listGroups
  , findGroup
  , recursivelyDeleteGroup

  , GroupDetails (..)
  , listGroupDetails

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
  , findJob
  , listJobs

  , JobSummary (..)
  , jobSummary
  , retryJobs
  , sumJobSummary

  -- * Worker
  , WorkerQuery (..)
  , Worker (..)
  , WorkerId
  , listWorkers
  , upsertWorker

  , WorkerDetails (..)
  , listWorkerDetails

  -- * Work
  , WorkQuery (..)
  , Work (..)
  , WorkId
  , createWork
  , startWorkOnAvailableJob
  , finishWorkWithResult
  , listWork

  , WorkDetails (..)
  , listWorkDetails

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
import           Data.Pool
import Data.Monoid
import qualified Data.List as List

-- containers
import qualified Data.Map.Strict as Map
import Data.Map.Merge.Strict as Map

-- rio
import           RIO hiding ((^.), on, set, isNothing)
import           RIO.Time
import qualified RIO.Text as Text


-- persist
import qualified Database.Persist.Sql as P
import           Database.Persist.TH

-- esqueleto
import           Database.Esqueleto

-- aeson
import           Data.Aeson.TH

-- middleman
import           Middleman.Server.Exception
import           Nix.Types
import           Middleman.Server.Model.Extra
import           TH

share
  [ mkPersist sqlSettings
  , mkDeleteCascade sqlSettings
  , mkMigrate "migrateAll"]
  [persistLowerCase|
  Group json
    name Text
    timeout Double
    UniqueGroupName name
    deriving Show Generic Ord Eq

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
    deriving Show Generic Ord Eq

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
    deriving Show Generic Eq
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
  P.insertUniqueEntity grp
    `orFail` ItemAlreadyExists grp

increaseTimeoutOfGroup ::
  GroupId -> Double  -> DB (Entity Group)
increaseTimeoutOfGroup groupId dp =
  Entity groupId <$>
    P.updateGet groupId [ GroupTimeout P.+=. dp]

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

data GroupDetails = GroupDetails
  { groupDId :: GroupId
  , groupDName :: Text
  , groupDTimeout :: Double
  , groupDJobSummary :: JobSummary
  } deriving (Show)

listGroupDetails ::
  GroupQuery -> DB [GroupDetails]
listGroupDetails _ = do
  jobs <- select $ from $
    \((((group `LeftOuterJoin` jd) `LeftOuterJoin` job) `LeftOuterJoin` work) `LeftOuterJoin` result) -> do
    on ( work ?. WorkResultId ==. just (result ?. ResultId) )
    on ( job ?. JobWorkId ==. just (work ?. WorkId) )
    on ( job ?. JobDescId ==. jd ?. JobDescriptionId )
    on ( just (group ^. GroupId) ==. jd ?. JobDescriptionGroupId )
    groupBy ( group ^. GroupId, job ?. JobWorkId ==. just(work ?. WorkId), result ?. ResultSuccess )
    return ( group
           , just (job ?. JobWorkId ==. just (work ?. WorkId))
           , result ?. ResultSuccess
           , countRows)

  let
    grp =
      mapGroupBy
      (\(g, Value run, Value r, Value cnt) -> (g, [(maybe False (const True) run, r, cnt)])) jobs
    res =
      map (\(Entity key group, lst) ->
             GroupDetails key (groupName group) (groupTimeout group) (mkJobSummary lst)
          )
      $ Map.toAscList grp

  return res

mapGroupBy :: (Monoid m, Ord k) => (a -> (k, m)) -> [a] -> Map.Map k m
mapGroupBy f lst =
  Map.fromListWith mappend $ map f lst


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
 , hasGroupId :: Maybe GroupId
 , isOlderThan :: Maybe UTCTime
 , hasSuccessState :: Maybe (Maybe Success)
 } deriving (Show, Eq)

instance Semigroup JobQuery where
  (<>) a b = JobQuery
     (hasJobDescriptionId b <|> hasJobDescriptionId a)
     (hasGroupId b <|> hasGroupId a)
     (isOlderThan b <|> isOlderThan a)
     (hasSuccessState b <|> hasSuccessState a)

instance Monoid JobQuery where
  mempty = JobQuery Nothing Nothing Nothing Nothing

findJob ::
  JobId -> DB (Maybe (Entity Job))
findJob jobId = do
  P.getEntity jobId

listJobs ::
  JobQuery -> DB [Entity Job]
listJobs ( JobQuery {..} ) = do
  let query =
        maybe [] (\jd -> [ JobDescId P.==. jd]) hasJobDescriptionId
  P.selectList query []

retryJobs ::
  JobQuery -> DB Int
retryJobs (JobQuery {..}) =
  let
    (inputs, wheres) = fold
      [ foldMap
        (\case
            Just sc -> ([ P.toPersistValue sc ], ["r.success = ?"])
            Nothing -> ([], ["w.result_id IS NULL"])
        )
        hasSuccessState
      , foldMap
        (\gId -> ( P.keyToValues gId , ["jd.group_id = ?"]))
        hasGroupId
      , foldMap
        (\date -> ( [ P.toPersistValue date], ["w.started < ?"]))
        isOlderThan
      ]
    sql = Text.intercalate " "
          [ "UPDATE job SET work_id =DEFAULT FROM job as j"
          , "INNER JOIN work as w ON j.work_id = w.id"
          , "INNER JOIN job_description as jd ON j.desc_id = jd.id"
          , "LEFT OUTER JOIN result as r ON w.result_id = r.id"
          , "WHERE ("
            <> Text.intercalate " AND " wheres
            <> ")"
          ]
  in do
    fmap fromIntegral (rawExecuteCount sql inputs)

    --(jd.group_id = ? AND j.id = job.id AND w.started < ? AND w.result_id IS NULL)"
data JobSummary = JobSummary
  { jobSActive :: Int
  , jobSSuccess :: Int
  , jobSTimeout :: Int
  , jobSFailed :: Int
  , jobSWaiting :: Int
  } deriving (Show, Eq)


mkJobSummary :: [(Bool, Maybe Success, Int)] -> JobSummary
mkJobSummary jobs = do
  JobSummary
    { jobSActive  = findOrZero (True, Nothing)
    , jobSSuccess = findOrZero (True, Just Succeded)
    , jobSTimeout  = findOrZero (True, Just Timeout)
    , jobSFailed = findOrZero (True, Just Failed)
    , jobSWaiting = findOrZero (False, Nothing)
    }
  where
    findOrZero v = Map.findWithDefault 0 v sucCount
    sucCount =
      foldMap (\(run, succ, c) -> Map.singleton (run, succ) c) jobs

sumJobSummary :: [JobSummary] -> JobSummary
sumJobSummary js =
  JobSummary
  { jobSActive = sum $ map jobSActive js
  , jobSSuccess = sum $ map jobSSuccess js
  , jobSTimeout = sum $ map jobSTimeout js
  , jobSFailed = sum $ map jobSFailed js
  , jobSWaiting = sum $ map jobSWaiting js
  }

deriveJSON (def 4) ''JobSummary

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

  return . mkJobSummary . map (\(Value a, Value b, Value c) ->
                                 (maybe False (const True) a, b, c)) $ jobs


data WorkerQuery = WorkerQuery
  { hasWorkerName :: Maybe Text
  } deriving (Show, Eq)

listWorkers ::
  WorkerQuery -> DB [Entity Worker]
listWorkers (WorkerQuery {..}) = do
  let query =
        maybe [] (\name -> [ WorkerName P.==. name ]) hasWorkerName
  P.selectList query [ P.Asc WorkerName ]

data WorkerDetails = WorkerDetails
  { workerDId :: WorkerId
  , workerDName :: Text
  , workerDActiveJobs :: Int
  , workerDCompletedJobs :: [ (UTCTime, UTCTime) ]
  } deriving (Show, Eq)

listWorkerDetails ::
  UTCTime -> WorkerQuery -> DB [WorkerDetails]
listWorkerDetails time _ = do
  active <- select $ from $ \(worker `LeftOuterJoin` work `LeftOuterJoin` result) -> do
    on ( work ?. WorkResultId ==. just (result ?. ResultId) )
    on ( work ?. WorkWorkerId ==. just (worker ^. WorkerId) )
    groupBy (worker ^. WorkerId)
    where_ (isNothing (work ?. WorkResultId ))
    return (worker, countRows)

  let actv :: Map.Map (Entity Worker) (Sum Int) =
        mapGroupBy (\(r, Value cnt) -> (r, Sum cnt)) active

  succeded
    <- select $ from $ \(worker `InnerJoin` work `InnerJoin` result) -> do
      on ( work ^. WorkResultId ==. just (result ^. ResultId) )
      on ( work ^. WorkWorkerId ==. worker ^. WorkerId)
      where_ (result ^. ResultSuccess ==. val Succeded
            &&. result ^. ResultEnded >=. val time)
      return (worker
             , work ^. WorkStarted
             , result ^. ResultEnded
             )

  let succ :: Map.Map (Entity Worker) [(UTCTime, UTCTime)] =
        Map.map (flip appEndo [])
        . mapGroupBy (\(r, Value t, Value cnt) -> (r, Endo ((t, cnt):)))
        $ succeded

  let res =
        Map.merge
           (Map.mapMissing
             (\(Entity key worker) x ->
                WorkerDetails key (workerName worker) (getSum x) []))
           (Map.mapMissing
            (\(Entity key worker) lst ->
                WorkerDetails key (workerName worker) 0 lst))
           (Map.zipWithMatched
             (\(Entity key worker) x lst ->
                 WorkerDetails key (workerName worker) (getSum x) lst ))
           actv
           succ

  return $ Map.elems res


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

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust = flip (maybe (return ()))

listWorkDetails ::
  WorkQuery -> DB [WorkDetails]
listWorkDetails (WorkQuery{..}) = do
  lst <- select $ from $ \(
    (work `InnerJoin` job `InnerJoin` jobd `InnerJoin` grp) `LeftOuterJoin` result) -> do
    on (result ?. ResultId ==. work ^. WorkResultId)
    on (jobd ^. JobDescriptionGroupId ==. grp ^. GroupId)
    on (job ^. JobDescId ==. jobd ^. JobDescriptionId )
    on (work ^. WorkJobId ==. job ^. JobId )
    whenJust hasWorkId $ \workId ->
      where_ ( work ^. WorkId ==. val workId )
    whenJust isAfterDate $ \date ->
      where_ ( work ^. WorkStarted >=. val date)
    return (work, jobd, grp, result)

  return . flip List.map lst $ \(Entity workId work, jobd, grp, result) ->
    WorkDetails
      { workDetailsId = workId
      , workDetailsStarted = workStarted work
      , workDetailsWorkerId = workWorkerId work
      , workDetailsJobDescription = jobd
      , workDetailsGroup = grp
      , workDetailsResult = (fmap entityVal result)
      }

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
    counts <- countWorks (workJobId work)
    when (counts < 10) $ do
      P.updateWhere
        [ JobId P.==. workJobId work]
        [ JobWorkId P.=. Nothing ]

countWorks ::
  JobId -> DB Int
countWorks jobId = do
  P.count [ WorkJobId P.==. jobId ]

data WorkQuery = WorkQuery
  { hasWorkId :: Maybe WorkId
  , hasJobId :: Maybe JobId
  , isAfterDate :: Maybe UTCTime
  } deriving (Show, Eq)

instance Semigroup WorkQuery where
  (<>) a b = WorkQuery
     (hasWorkId b <|> hasWorkId a)
     (hasJobId b <|> hasJobId a)
     (isAfterDate b <|> isAfterDate a)

instance Monoid WorkQuery where
  mempty = WorkQuery Nothing Nothing Nothing

listWork ::
  WorkQuery -> DB [Entity Work]
listWork WorkQuery {..} = do
  let filters = concat
        [ hasWorkId `mif` \wid-> WorkId P.==. wid
        , hasJobId `mif` \jid -> WorkJobId P.==. jid
        , isAfterDate `mif` \date -> WorkStarted P.>=. date
        ]
  P.selectList filters [ P.Asc WorkStarted ]
  where
    mif :: Maybe a -> (a -> b) -> [b]
    i `mif` f = foldMap ((:[]) . f) i

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
  display (WorkQuery {..}) =
    fold $
    [ "WorkQuery("
    , foldMap (\name -> "has name = " <> display name) hasWorkId
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
