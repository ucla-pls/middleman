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

-- | Create a group of jobs
createGroup ::
  DB.Group -> Server m (DB.Entity DB.Group)
createGroup grp =
  DB.inDB ( DB.createGroup grp )

-- | Remove a group and all it's corresponding jobs.
deleteGroup ::
  DB.GroupId -> Server m ()
deleteGroup groupId = do
  jobs <- DB.inDB ( DB.jobDescriptionsWithGroup groupId)
  forM_ jobs $ \(DB.entityVal -> jobDesc) -> do
    Nix.removeGCRoot ( relativeLinkOfJobDescription jobDesc )
    Nix.removeGCRoot ( relativeLinkOfJob jobDesc )
  DB.inDB ( DB.recursivelyDeleteGroup groupId )

-- | There can only be one job description, per derivation. You will
-- have to delete any old job descriptions to re-run.
submitJobDescription ::
  DB.JobDescription -> Server m (DB.Entity DB.JobDescription)
submitJobDescription desc = do
  Nix.ensureGCRoot
    ( relativeLinkOfJobDescription desc )
    ( Nix.inStore . Derivation $ DB.jobDescriptionDerivation desc )
  jobDesc <- DB.inDB ( DB.createJobDescription desc )
  return jobDesc

publishJob ::
  DB.JobDescriptionId -> Server m (DB.Entity DB.Job)
publishJob descId = do
  desc <- DB.inDB ( DB.findJobDescription descId )

  -- Validation
  handleNix $ Nix.validateLink ( relativeLinkOfJobDescription desc )

  -- Output link definition
  output <- handleNix $
    Nix.readDerivationOutput
    ( Derivation $ DB.jobDescriptionDerivation desc )
  Nix.ensureGCRoot ( relativeLinkOfJob desc ) output

  -- Creation
  DB.inDB ( DB.createJob (DB.Job descId Nothing output) )

-- * Work Creation

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
getWorkDescription ::
  DB.WorkId -> Server m (DB.Entity DB.Work, DB.Entity DB.JobDescription, DB.Entity DB.Group)
getWorkDescription workId =
  DB.inDB ( DB.findWorkDescription workId )

-- | Complete work
finishWork ::
  DB.WorkId -> DB.Result -> Server m ()
finishWork workId result = do
  ((DB.entityVal -> work), (DB.entityVal -> desc), _) <-
    getWorkDescription workId

  let (start, end) = (DB.workStarted work, DB.resultEnded result)
  when (start >= end) $
    throwIO . InvalidInput $
      "The end time should be later than the start time: "
      ++ show start ++ " >= " ++ show end

  when (DB.resultSuccess result == DB.Succeded) $ do
    -- Validation
    handleNix $ Nix.validateLink ( relativeLinkOfJob desc )

  DB.inDB ( DB.finishWorkWithResult workId result )

-- -- ** Others

relativeLinkOfJobDescription ::
  DB.JobDescription -> FilePath
relativeLinkOfJobDescription desc =
  RIO.FilePath.takeFileName
    ( Nix.inStore
      ( Derivation $ DB.jobDescriptionDerivation desc) )

relativeLinkOfJob ::
  DB.JobDescription -> FilePath
relativeLinkOfJob desc =
  RIO.FilePath.dropExtensions
    ( relativeLinkOfJobDescription desc )
  ++ "-output"
