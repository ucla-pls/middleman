{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-
Module      : Middleman.DTO
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : BSD
Maintainer  : kalhuage@cs.ucla.edu
Stability   : experimental
Description : The data transfer objects of middleman
-}

module Middleman.DTO
  ( DB.Entity (..)
  , keyToInt

  , DB.Group (..)
  , DB.GroupId

  , DB.JobDescription (..)
  , DB.JobDescriptionId

  , DB.Job (..)
  , DB.JobId

  , DB.Worker (..)
  , DB.WorkerId
  , NewWorker (..)

  , DB.Work (..)
  , DB.WorkId
  , DB.WorkDetails (..)

  , DB.Success (..)

  , Info (..)

  , AddTime (..)
  )
  where

-- base
import Data.Functor

-- rio
import RIO
import RIO.Text as Text

-- aeson
import Data.Aeson
import Data.Aeson.TH

-- scotty
import Web.Scotty.Trans

-- middleman
import qualified Middleman.Server.Model as DB
import Network.HTTP.Client.Helper (Writeable (..))
import TH

import Nix.Types as Nix

data Info =
  Info
  { infoStoreUrl :: !Nix.Store
  } deriving (Show, Generic)

deriveJSON (def 4) ''Info

data NewWorker =
  NewWorker
  { newWorkerName :: !Text
  }

deriveJSON (def 9) ''NewWorker

data AddTime =
  AddTime
  { addTime :: !Double
  }

deriveJSON (def 0) ''AddTime

keyToInt :: (DB.ToBackendKey DB.SqlBackend r) => DB.Key r -> Int64
keyToInt = DB.fromSqlKey

instance (Parsable DB.GroupId) where
  parseParam txt =
    DB.toSqlKey <$> parseParam txt

instance (Parsable DB.JobDescriptionId) where
  parseParam txt =
    DB.toSqlKey <$> parseParam txt

instance (Parsable DB.JobId) where
  parseParam txt =
    DB.toSqlKey <$> parseParam txt

instance (Parsable DB.WorkerId) where
  parseParam txt =
    DB.toSqlKey <$> parseParam txt

instance (Parsable DB.WorkId) where
  parseParam txt =
    DB.toSqlKey <$> parseParam txt

instance (Parsable DB.Success) where
  parseParam txt =
    case txt of
      "retry" -> return $ DB.Retry
      "succeded" -> return $ DB.Succeded
      "failed" -> return $ DB.Failed
      "timeout" -> return $ DB.Timeout
      _ -> Left "Could not read success"

instance (Writeable DB.GroupId) where
  writeParam = writeParam . DB.fromSqlKey

instance (Writeable DB.JobDescriptionId) where
  writeParam = writeParam . DB.fromSqlKey

instance (Writeable DB.WorkerId) where
  writeParam = writeParam . DB.fromSqlKey

instance (Writeable DB.WorkId) where
  writeParam = writeParam . DB.fromSqlKey

instance (Writeable DB.Success) where
  writeParam = \case
    DB.Retry -> "retry"
    DB.Succeded -> "succeded"
    DB.Failed -> "failed"
    DB.Timeout -> "timeout"

instance (FromJSON Nix.Store) where
  parseJSON = withText "Store"
    (return . Nix.storeFromUrl . Text.unpack)

instance (ToJSON Nix.Store) where
  toJSON = String . Text.pack . Nix.toStoreUrl
