{-# LANGUAGE TemplateHaskell #-}
{-
Module      : Middleman.Server.Model.Extra
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : BSD
Maintainer  : kalhuage@cs.ucla.edu
Stability   : experimental
Description : Extra types used in the model
-}
module Middleman.Server.Model.Extra
  where

-- rio
import RIO
import RIO.Text

-- persist
import Database.Persist.Sql

-- aeson
import Data.Aeson.TH
import Data.Aeson

-- middleman
import Nix.Tools

-- * Success

data Success =
  Succeded
  | Failed
  | Retry
  deriving (Show, Generic, Eq, Ord)

successToInt :: Success -> Int
successToInt = \case
  Succeded -> 0
  Failed -> 1
  Retry -> 2

successFromInt :: Int -> Maybe Success
successFromInt = \case
  0 -> Just Succeded
  1 -> Just Failed
  2 -> Just Retry
  _ -> Nothing

deriveJSON defaultOptions ''Success

instance PersistField Success where
  toPersistValue =
    PersistInt64 . fromIntegral . successToInt
  fromPersistValue = \case
    PersistInt64 n ->
      maybe
        (Left $ "Bad success value, required 0, 1, or 2, was" <> pack (show n))
        Right
        (successFromInt . fromIntegral $ n)
    x -> Left $ "Bad success value, required int, was " <> pack (show x)

instance PersistFieldSql Success where
  sqlType _ = SqlInt32

instance PersistField Derivation where
  toPersistValue = PersistText . derivationName
  fromPersistValue = \case
    PersistText txt -> Right ( Derivation txt )
    _ -> Left "Bad derivation expected text"

instance PersistFieldSql Derivation where
  sqlType _ = SqlString

instance ToJSON Derivation where
  toJSON = String . derivationName

instance FromJSON Derivation where
  parseJSON = withText "Derivation" $
    return . Derivation
