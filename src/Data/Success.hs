{-# LANGUAGE TemplateHaskell #-}
module Data.Success where

import Data.Maybe
import Data.Int
import Data.Eq
import Text.Show

import GHC.Generics
import Data.Aeson.TH

data Success =
  Succeded
  | Failed
  | Retry
  deriving (Show, Generic, Eq)

toInt :: Success -> Int
toInt = \case
  Succeded -> 0
  Failed -> 1
  Retry -> 2

fromInt :: Int -> Maybe Success
fromInt = \case
  0 -> Just Succeded
  1 -> Just Failed
  2 -> Just Retry
  _ -> Nothing

deriveJSON defaultOptions ''Success
