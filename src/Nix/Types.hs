{-
Module      : Nix.Types
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : BSD
Maintainer  : kalhuage@cs.ucla.edu
Stability   : experimental
Description : Nix types
-}
module Nix.Types
  where

-- rio
import RIO
import RIO.Text as Text
import RIO.FilePath

-- | The url to the store.
newtype Store =
  Store { toStoreUrl :: String }
  deriving (Show, Eq)

instance IsString Store where
  fromString = Store

storeFromUrl :: String -> Store
storeFromUrl = Store

-- * InStore

-- | A data structure that is located in the store.
class InStore a where
  inStore :: a -> FilePath
  fromStore :: FilePath -> a


-- * Types

newtype Derivation =
  Derivation { derivationName :: Text }
  deriving (Show, Eq)

instance InStore Derivation where
  inStore drv =
    "/nix/store" </> Text.unpack (derivationName drv) <.> "drv"
  fromStore filepath =
    Derivation . Text.pack . takeBaseName $ filepath

newtype OutputPath =
  OutputPath { storeBaseName :: FilePath }
  deriving (Show, Eq)


instance InStore OutputPath where
  inStore path =
    "/nix/store" </> storeBaseName path
  fromStore filepath =
    OutputPath $ takeFileName filepath

data AnyStorePath
  = StoreDerivation !Derivation
  | StoreOutputPath !OutputPath
  deriving (Show, Eq)

instance InStore AnyStorePath where
  inStore path =
    case path of
      StoreOutputPath op -> inStore op
      StoreDerivation drv -> inStore drv
  fromStore path
    | takeExtensions path == ".drv" =
        StoreDerivation (fromStore path)
    | otherwise =
        StoreOutputPath (fromStore path)
