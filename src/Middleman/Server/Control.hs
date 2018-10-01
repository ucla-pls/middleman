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

-- -- | There can only be one job description, per derivation. You will
-- -- have to delete any old job descriptions to re-run.
-- submitJobDescription ::
--   (Monad m)
--   => JobDescription
--   -> m JobDescription
-- submitJobDescription desc = do
--   jobDesc <- DB.createJobDescription desc
--   Nix.ensureGCRoot ( relativeLinkOfJobDescription desc ) drv
--   return jobDesc

-- publishJob ::
--   (Monad m)
--   => JobDescriptionId
--   -> m Job
-- publishJob descId = do
--   desc <- DB.findJobDescription descId

--   -- Validation
--   handleNix $ Nix.validateLink ( relativeLinkOfJobDescription desc )

--   -- Output link definition
--   output <- handleNix $ Nix.readDerivationOutput ( jobDerivation desc )
--   Nix.ensureGCRoot ( relativeLinkOfJob desc ) output

--   -- Creation
--   DB.createJob descId output

-- -- * Work Creation

-- -- | Create or update a worker.
-- upsertWorker ::
--   (Monad m)
--   => WorkerName
--   -> IpAdress
--   -> m (DB.Entity Worker)
-- upsertWorker name ipaddress = do
--   worker <- DB.upsertWorker (Worker name ipadress)
--   return worker

-- -- | Given a WorkerId create work
-- findWork ::
--   (Monad m)
--   => WorkerId
--   -> m (Maybe WorkDescription)
-- findWork workerId = do
--   timt <- getCurrentTime
--   (workId, jobDesc, group) <- DB.createWorkWithWorker time workerId
--   return $ WorkDescription
--     { workId = workId
--     , derivation = jobDerivation jobDesc
--     , timeout = groupTimeout group
--     }

-- -- | Complete work
-- finishWork ::
--   (Monad m)
--   => WorkId
--   -> Success
--   -> m ()
-- finishWork workId succ = do
--   (work, jobDesc) <- DB.findJobFromWork workId

--   when (succ == Succeded) $ do
--     -- Validation
--     handleNix $ Nix.validateLink ( relativeLinkOfJob desc )

--   DB.finishWork time workId

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
