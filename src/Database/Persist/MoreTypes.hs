{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | More peristent types
module Database.Persist.MoreTypes where

import RIO
import qualified RIO.Text as Text
import           Database.Persist.Sql

import qualified Data.Success

instance PersistField Data.Success.Success where
  toPersistValue =
    PersistInt64 . fromIntegral . Data.Success.toInt
  fromPersistValue = \case
    PersistInt64 n ->
      maybe
        (Left $ "Bad success value, required 0, 1, or 2, was" <> Text.pack (show n))
        Right
        (Data.Success.fromInt . fromIntegral $ n)
    x -> Left $ "Bad success value, required int, was " <> Text.pack (show x)

instance PersistFieldSql Data.Success.Success where
  sqlType _ = SqlInt32
