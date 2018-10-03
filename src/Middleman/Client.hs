{-
Module      : Middleman.Client
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : BSD
Maintainer  : kalhuage@cs.ucla.edu
Stability   : experimental
Description : Bindings for accessing the server
-}
module Middleman.Client
  ( getInfo

  , getGroupWithName

  , postJobDescription
  , postJobDescriptions
  , publishJobDescription

  , getJobWithDescription

  -- * Rexports
  , HasServerAccess (..)
  , Server (..)
  , Manager
  )
  where

-- http-types
import Network.HTTP.Types.Status

-- http-client
import Network.HTTP.Client

-- rio
import RIO
import RIO.List as List

-- middleman
import Network.HTTP.Client.Helper
import Middleman.DTO


type Client a =
  forall m env. (MonadReader env m, HasServerAccess env, MonadUnliftIO m)
   => m a

-- * Access commands

-- | Get the info
getInfo :: Client Info
getInfo =
  get . expectOk . json $
    "api" <?> []

getGroup :: GroupId -> Client (Maybe (Entity Group))
getGroup gprid = do
  get . expectNoContent . json $
    "api" </> "groups" </> value gprid <?> []

getGroupWithName :: Text -> Client (Maybe (Entity Group))
getGroupWithName name = do
  lst <- get . expectOk . json $
    "api" </> "groups" <?> "name" =. name
  return (List.headMaybe lst)

postJobDescription :: JobDescription -> Client (Entity JobDescription)
postJobDescription jd =
  post jd . expectCreated . json $
    "api" </> "job-descriptions" <?> []

postJobDescriptions :: [JobDescription] -> Client [Entity JobDescription]
postJobDescriptions jd =
  post jd . expectOk . json $
    "api" </> "job-descriptions" <?> []

publishJobDescription :: JobDescriptionId -> Client (Entity Job)
publishJobDescription jid =
  post () . expectCreated . json $
    "api" </> "job-descriptions" </> value jid </> "publish" <?> []

getJobWithDescription :: JobDescriptionId -> Client (Maybe (Entity Job))
getJobWithDescription jdid = do
  lst <- get . expectOk . json $
    "api" </> "jobs" <?> "desc" =. jdid
  return (List.headMaybe lst)

getWorker :: Text -> Client (Maybe (Entity Worker))
getWorker name = do
  lst <- get . expectOk . json $
    "api" </> "workers" <?> "name" =. name
  return (List.headMaybe lst)

postWorker :: Text -> Client (Entity Worker)
postWorker name =
  post ( NewWorker name ) . expectCreated . json $
    "api" </> "workers" <?> []

ensureWorker :: Text -> Client (Entity Worker)
ensureWorker txt =
  getWorker txt >>= \case
    Just worker ->
      return worker
    Nothing -> do
      postWorker txt

pullWork ::
  WorkerId -> Client (Maybe WorkDetails)
pullWork workerId =
  post () . expectNoContent . json $
    "api" </> "workers" </> value workerId </> "work" <?> []

