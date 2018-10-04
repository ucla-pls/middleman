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
import RIO.FilePath
import RIO.Process
import RIO.Time (getCurrentTime)

-- middleman
import Middleman.Server.Exception
import qualified Middleman.Server.Model as DB
import Nix.Tools as Nix

-- * Server interface

type Server m a =
  forall env.
  ( MonadReader env m
  , Nix.HasGCRoot env
  , DB.HasSqlPool env
  , HasLogFunc env
  , HasProcessContext env
  , MonadUnliftIO m
  ) => m a

-- * Job Creation

-- | List Groups
listGroups ::
  Maybe Text -> Server m [DB.Entity DB.Group]
listGroups mname = do
  DB.inDB ( DB.listGroups mname )

-- | Find Groups
findGroup ::
  DB.GroupId -> Server m (Maybe (DB.Entity DB.Group))
findGroup groupId = do
  DB.inDB ( DB.findGroup groupId )

-- | Create a group of jobs
createGroup ::
  DB.Group -> Server m (DB.Entity DB.Group)
createGroup grp =
  DB.inDB ( DB.createGroup grp )

-- | Remove a group and all it's corresponding jobs.
deleteGroup ::
  DB.GroupId -> Server m ()
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

-- | There can only be one job description, per derivation. You will
-- have to delete any old job descriptions to re-run.
submitJobDescription ::
  DB.JobDescription -> Server m (Bool, DB.Entity DB.JobDescription)
submitJobDescription desc = do
  Nix.ensureGCRoot
    ( Nix.inStore $ DB.jobDescriptionDerivation desc )
    ( relativeLinkOfJobDescription desc )
  jobDesc <- DB.inDB ( DB.upsertJobDescription desc )
  return jobDesc

findJobDescription ::
  DB.JobDescriptionId -> Server m (Maybe (DB.Entity DB.JobDescription))
findJobDescription descId =
  DB.inDB ( DB.findJobDescription descId )

listJobDescriptions ::
  Server m [DB.Entity DB.JobDescription]
listJobDescriptions =
  DB.inDB ( DB.listJobDescriptions )

publishJob ::
  DB.JobDescriptionId -> Server m (DB.Entity DB.Job)
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
    ( takeBaseName output )
    ( relativeLinkOfJob desc )

  -- Creation
  DB.inDB ( DB.createJob (DB.Job descId Nothing output) )

listJobs ::
  Maybe DB.JobDescriptionId
  -> Server m [DB.Entity DB.Job]
listJobs q = do
  DB.inDB ( DB.listJobs q )

-- * Work Creation

listWorkers ::
  Maybe Text -> Server m [DB.Entity DB.Worker]
listWorkers query =
  DB.inDB ( DB.listWorkers query )

-- | Create or update a worker.
upsertWorker ::
  DB.Worker -> Server m (DB.Entity DB.Worker)
upsertWorker worker = do
  DB.inDB ( DB.upsertWorker worker )

-- | Given a WorkerId create work
startWork ::
  DB.WorkerId -> Server m (Maybe (DB.Entity DB.Work))
startWork workerId = do
  time <- getCurrentTime
  DB.inDB ( DB.startWorkOnAvailableJob time workerId )

-- | Given a `DB.WorkId` find the neseary information to compute it.
findWorkDetails ::
  DB.WorkId -> Server m (Maybe DB.WorkDetails)
findWorkDetails workId = do
  DB.inDB ( DB.findWorkDetails workId )

-- | Complete work
finishWork ::
  DB.WorkId -> DB.Success -> Server m ()
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

listWork ::
  Server m [DB.Entity DB.Work]
listWork =
  DB.inDB DB.listWork

-- -- ** Others

relativeLinkOfJobDescription ::
  DB.JobDescription -> FilePath
relativeLinkOfJobDescription desc =
  RIO.FilePath.takeFileName
    ( Nix.inStore ( DB.jobDescriptionDerivation desc ) )

relativeLinkOfJob ::
  DB.JobDescription -> FilePath
relativeLinkOfJob desc =
  RIO.FilePath.dropExtensions
    ( relativeLinkOfJobDescription desc )
