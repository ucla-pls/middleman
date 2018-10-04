{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
import RIO.Text as Text

-- persist
import Database.Persist.Sql

-- aeson
import Data.Aeson.TH
import Data.Aeson

-- middleman
import Nix.Types

-- * Success

data Success =
  Succeded
  | Failed
  | Retry
  | Timeout
  deriving (Show, Generic, Eq, Ord)

successToInt :: Success -> Int
successToInt = \case
  Succeded -> 0
  Failed -> 1
  Retry -> 2
  Timeout -> 3

successFromInt :: Int -> Maybe Success
successFromInt = \case
  0 -> Just Succeded
  1 -> Just Failed
  2 -> Just Retry
  3 -> Just Timeout
  _ -> Nothing

deriveJSON defaultOptions ''Success

instance PersistField Success where
  toPersistValue =
    PersistInt64 . fromIntegral . successToInt
  fromPersistValue = \case
    PersistInt64 n ->
      maybe
        (Left $ "Bad success value, required 0, 1, 2, or 3 was" <> pack (show n))
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

instance PersistField OutputPath where
  toPersistValue = PersistText . Text.pack . storeBaseName
  fromPersistValue = \case
    PersistText txt -> Right ( OutputPath (Text.unpack txt))
    _ -> Left "Bad outputPath expected text"

instance PersistFieldSql OutputPath where
  sqlType _ = SqlString

instance ToJSON OutputPath where
  toJSON = String . Text.pack . storeBaseName

instance FromJSON OutputPath where
  parseJSON = withText "OutputPath" $
    return . OutputPath . Text.unpack
