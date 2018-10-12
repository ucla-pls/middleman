{-
Module      : Middleman.Server.Control
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : BSD
Maintainer  : kalhuage@cs.ucla.edu
Stability   : experimental
Description : The control logic of the server

This module contains the business logic of the middleman server.

The module uses exceptions, and all functions can throw a
`MiddlemanServerException`.

-}
module Middleman.Server.Control
  where

-- rio
import RIO
import RIO.Time
import RIO.List as List
import RIO.Text as Text
import RIO.Process
import RIO.Time (getCurrentTime)

-- middleman
import Middleman.Server.Exception
import qualified Middleman.Server.Model as DB
import Nix.Tools as Nix

-- * Server interface

type Server env a =
  forall m.
  ( MonadReader env m
  , HasLogFunc env
  , DB.HasSqlPool env
  , MonadUnliftIO m
  ) => m a

-- * Groups

-- | List Groups
listGroups ::
  DB.GroupQuery -> Server env [DB.Entity DB.Group]
listGroups query = do
  groups <- DB.inDB ( DB.listGroups query )
  logDebug $
    "Found " <> display (List.length groups)
    <> " groups matching query " <> display query <> "."
  return groups

-- | List Groups
listGroupDetails ::
  DB.GroupQuery -> Server env [DB.GroupDetails]
listGroupDetails query = do
  groups <- DB.inDB ( DB.listGroupDetails query )
  logDebug $
    "Found " <> display (List.length groups)
    <> " group details matching query " <> display query <> "."
  return groups


-- | Find Groups
findGroup ::
  DB.GroupId -> Server env (Maybe (DB.Entity DB.Group))
findGroup groupId = do
  egroup <- DB.inDB ( DB.findGroup groupId )
  logDebug $
    maybe "Didn't find" (const "Found") egroup
    <> " a group when looking for " <> display groupId <> "."
  return egroup

-- | Create a group of jobs
createGroup ::
  DB.Group -> Server env (DB.Entity DB.Group)
createGroup grp = do
  egroup <- DB.inDB ( DB.createGroup grp )
  logDebug $ "Created new group " <> display (DB.entityKey egroup) <> "."
  return egroup

-- | Remove a group and all it's corresponding jobs.
deleteGroup ::
  (HasGCRoot env)
  => DB.GroupId -> Server env ()
deleteGroup groupId = do
  logDebug $ "Deleting group " <> displayShow groupId
  jobs <- DB.inDB ( DB.jobDescriptionsWithGroup groupId)
  forM_ jobs $ \(DB.entityVal -> jobDesc) -> do
    logDebug $ "Removing links from " <> displayShow jobDesc
    Nix.removeGCRoot ( relativeLinkOfJobDescription jobDesc )
    Nix.removeGCRoot ( relativeLinkOfJob jobDesc )
  logDebug $ "Recursively deleting group"
  DB.inDB ( DB.recursivelyDeleteGroup groupId )
  logDebug $ "done"

-- | Increase timout of group
increaseTimeoutOfGroup ::
  DB.GroupId -> Double -> Server env Bool
increaseTimeoutOfGroup groupId tdiff = do
  egroup <- DB.inDB ( DB.findGroup groupId )
  case egroup of
    Just grp -> do
      if (tdiff > 0)
        then do
        jobCount <- DB.inDB $ do
          jobCount <- DB.retryJobs (
            mempty { DB.hasGroupId = Just groupId
                   , DB.hasSuccessState = Just (Just DB.Timeout)}
            )
          _ <- DB.increaseTimeoutOfGroup groupId tdiff
          return jobCount
        logDebug $ "Updated timeout of group " <> display (DB.entityKey grp)
          <> " retried " <> display jobCount <> " timed-out jobs."
        return True
        else do
        logError $ "Group as timeout " <> display (DB.groupTimeout (DB.entityVal grp)) <> " greater than the increased timeout"
        return False
    Nothing -> do
      logError $ "No group with id " <> display groupId
      return False

retryOldGroup ::
  DB.GroupId -> Server env Int
retryOldGroup gid = do
  i <- findGroup gid >>= \case
    Just (DB.groupTimeout . DB.entityVal -> to') -> do
      t <- getCurrentTime
      let startTime = addUTCTime (realToFrac $ negate to') t
      DB.inDB (
        DB.retryJobs
          (mempty
           { DB.hasGroupId = Just gid
           , DB.isOlderThan = Just startTime
           , DB.hasSuccessState = Just Nothing
           })
        )
    Nothing ->
      return 0
  logDebug $ "Retrying " <> display i <> " old jobs."
  return i

-- | There can only be one job description, per derivation. You will
-- have to delete any old job descriptions to re-run.
submitJobDescription ::
  (HasGCRoot env)
  => DB.JobDescription
  -> Server env (Bool, DB.Entity DB.JobDescription)
submitJobDescription desc = do
  logDebug $ "Creating gc-roots for "
    <> displayShow (DB.jobDescriptionDerivation desc)
  Nix.ensureGCRoot
    ( DB.jobDescriptionDerivation desc )
    ( relativeLinkOfJobDescription desc )
  jobDesc <- DB.inDB ( DB.upsertJobDescription desc )
  logDebug $ "Successfully submitted "
    <> display (DB.entityKey (snd jobDesc)) <> "."
  return jobDesc

findJobDescription ::
  DB.JobDescriptionId
  -> Server env (Maybe (DB.Entity DB.JobDescription))
findJobDescription descId = do
  edesc <- DB.inDB ( DB.findJobDescription descId )
  logDebug $
    maybe "Didn't find" (const "Found") edesc
    <> "when looking for " <> display descId <> "."
  return edesc

listJobDescriptions ::
  DB.JobDescriptionQuery
  -> Server env [DB.Entity DB.JobDescription]
listJobDescriptions query = do
  descs <- DB.inDB ( DB.listJobDescriptions query )
  logDebug $
    "Found " <> display (List.length descs)
    <> " job descriptions matching query " <> display query <> "."
  return descs

publishJob ::
  (HasGCRoot env, HasProcessContext env)
  => DB.JobDescriptionId
  -> Server env (DB.Entity DB.Job)
publishJob descId = do
  (DB.entityVal -> desc) <- findJobDescription descId
    `orFail` ItemNotFoundException descId

  -- Validation
  handleNix $ Nix.validateLink ( relativeLinkOfJobDescription desc )

  -- Output link definition
  output <- handleNix $
    Nix.readDerivationOutput
    ( DB.jobDescriptionDerivation desc )

  Nix.ensureGCRoot
    output
    ( relativeLinkOfJob desc )

  -- Creation
  ejob <- DB.inDB ( DB.createJob (DB.Job descId Nothing output) )
  logDebug $ "Successfully published " <> display descId <> " as "
    <> display (DB.entityKey ejob) <> "."
  return ejob

listJobs ::
  DB.JobQuery
  -> Server m [DB.Entity DB.Job]
listJobs query = do
  jobs <- DB.inDB ( DB.listJobs query )
  logDebug $
    "Found " <> display (List.length jobs)
    <> " jobs matching query " <> display query <> "."
  return jobs

-- retryJob ::
--   DB.JobQuery
--   -> Server m [DB.Entity DB.Job]
-- retryJob query = do
--   jobs <- DB.inDB ( DB.retryJobs query )
--   logDebug $
--     "Found " <> display (List.length jobs)
--     <> " jobs matching query " <> display query <> "."
--   return jobs

jobSummary ::
  DB.JobQuery
  -> Server m DB.JobSummary
jobSummary query = do
  jobsum <- DB.inDB ( DB.jobSummary query )
  logDebug $
    "Found a jobs summary."
  return jobsum

-- -- * Work Creation

listWorkers ::
  DB.WorkerQuery -> Server env [DB.Entity DB.Worker]
listWorkers query = do
  workers <- DB.inDB ( DB.listWorkers query )
  logDebug $
    "Found " <> display (List.length workers)
    <> " workers matching query " <> display query <> "."
  return workers

listWorkerDetails ::
  UTCTime -> DB.WorkerQuery -> Server env [DB.WorkerDetails]
listWorkerDetails time query = do
  workerDetails <- DB.inDB ( DB.listWorkerDetails time query )
  logDebug $
    "Found " <> display (List.length workerDetails)
    <> " workerDetails matching query " <> display query <> "."
  return workerDetails

-- | Create or update a worker.
upsertWorker ::
  DB.Worker -> Server env (DB.Entity DB.Worker)
upsertWorker worker = do
  eworker <- DB.inDB ( DB.upsertWorker worker )
  logDebug $ "Created " <> display (DB.entityKey eworker) <>"."
  return eworker

-- | Given a WorkerId create work
startWork ::
  DB.WorkerId -> Server env (Maybe (DB.Entity DB.Work))
startWork workerId = do
  time <- getCurrentTime
  work <- DB.inDB ( DB.startWorkOnAvailableJob time workerId )
  case work of
    Just w ->
      logDebug $
        "Started " <> display (DB.entityKey w)
        <> " at " <>
        ( display . Text.pack $
          formatTime
          defaultTimeLocale
          (iso8601DateFormat (Just "%H:%M:%S"))
          time )
        <> "."
    Nothing ->
      logDebug "No more available jobs found."
  return work

-- | Given a `DB.WorkId` find the neseary information to compute it.
findWorkDetails ::
  DB.WorkId -> Server m (Maybe DB.WorkDetails)
findWorkDetails workId = do
  logDebug $ "Finding work details for " <> display workId <> "."
  DB.inDB ( List.headMaybe <$> DB.listWorkDetails (DB.WorkQuery $ Just workId ))

-- | Given a `DB.WorkId` find the neseary information to compute it.
listWorkDetails ::
  DB.WorkQuery -> Server m [DB.WorkDetails]
listWorkDetails query = do
  logDebug $ "Finding work details for " <> display query <> "."
  DB.inDB ( DB.listWorkDetails query )

-- | Complete work
finishWork ::
  (HasGCRoot env)
  => DB.WorkId
  -> DB.Success
  -> Server env ()
finishWork workId success = do
  time <- getCurrentTime
  let result = DB.Result time success

  w <- findWorkDetails workId `orFail` ItemNotFoundException workId

  let (start, end) = (DB.workDetailsStarted w, DB.resultEnded result)
  when (start >= end) $
    throwIO . InvalidInput $
      "The end time should be later than the start time: "
      ++ show start ++ " >= " ++ show end

  when (DB.resultSuccess result == DB.Succeded) $ do
    -- Validation
    handleNix $ Nix.validateLink
      ( relativeLinkOfJob (DB.entityVal (DB.workDetailsJobDescription w)))

  DB.inDB ( DB.finishWorkWithResult workId result )

  logDebug $ "Setting " <> display workId <> " to " <> displayShow success
    <> " at "
    <> ( display . Text.pack $
          formatTime
          defaultTimeLocale
          (iso8601DateFormat (Just "%H:%M:%S"))
          time )
    <> " a total of "
    <> display
    ( realToFrac (time `diffUTCTime` DB.workDetailsStarted w)
         :: Double
    )
    <> " seconds."

listWork ::
  DB.WorkQuery -> Server m [DB.Entity DB.Work]
listWork query = do
  work <- DB.inDB ( DB.listWork query )
  logDebug $
    "Found " <> display (List.length work)
    <> " work matching query " <> display query <> "."
  return work

-- -- ** Others

relativeLinkOfJobDescription ::
  DB.JobDescription -> FilePath
relativeLinkOfJobDescription =
  (++ ".drv") . relativeLinkOfJob

relativeLinkOfJob ::
  DB.JobDescription -> FilePath
relativeLinkOfJob =
  Text.unpack
  . DB.derivationName
  . DB.jobDescriptionDerivation
