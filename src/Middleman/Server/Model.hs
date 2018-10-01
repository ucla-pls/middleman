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

  -- * Group
  , Group (..)
  , GroupId
  , createGroup
  , recursivelyDeleteGroup

  -- * JobDescription
  , JobDescription (..)
  , JobDescriptionId
  , jobDescriptionsWithGroup

  -- * HasSqlPool
  , HasSqlPool (..)

  -- * Re-exports
  , Entity (..)
  )
  where

-- base?
import Data.Pool

-- rio
import RIO hiding ((^.))
import RIO.Text

-- persist
import qualified Database.Persist.Sql as P
import           Database.Persist.TH

-- esqueleto
import Database.Esqueleto

-- middleman
import           Middleman.Server.Exception

share
  [ mkPersist sqlSettings
  , mkDeleteCascade sqlOnlySettings
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

createGroup ::
  Group -> DB (Entity Group)
createGroup group = do
  P.insertUniqueEntity group `orFail` ItemAlreadyExists group

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
