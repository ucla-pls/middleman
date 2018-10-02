{-# LANGUAGE TemplateHaskell #-}
{-
Module      : Middleman.DTO
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : BSD
Maintainer  : kalhuage@cs.ucla.edu
Stability   : experimental
Description : The data transfer objects of middleman
-}

module Middleman.DTO
  ( DB.Entity

  , DB.Group (..)
  , DB.GroupId

  , DB.JobDescription (..)
  , DB.JobDescriptionId

  , DB.Worker (..)
  , DB.WorkerId

  , DB.Work (..)
  , DB.WorkId

  , DB.WorkDetails (..)
  , Info (..)
  )
  where

-- base
import Data.Functor

-- rio
import RIO
import RIO.Time

-- aeson
import Data.Aeson.TH

-- scotty
import Web.Scotty.Trans

-- middleman
import qualified Middleman.Server.Model as DB
import TH

data Info =
  Info
  { infoStoreAddress :: !String
  } deriving (Show, Generic)

deriveJSON (def 4) ''Info

instance (Parsable DB.GroupId) where
  parseParam txt =
    DB.toSqlKey <$> parseParam txt

instance (Parsable DB.JobDescriptionId) where
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
      _ -> Left "Could not read success"
