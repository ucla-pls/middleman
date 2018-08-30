{-# LANGUAGE TemplateHaskell #-}
module DTOs where

import RIO

import Data.Aeson.TH
import TH

data NewJobDTO = NewJobDTO
  { njdtoPath :: String
  , njdtoGroup :: String
  } deriving (Show)

data WorkRequestDTO = WorkRequestDTO
  { wrdtoHostName :: String
  } deriving (Show)

deriveJSON (def 5) ''NewJobDTO
deriveJSON (def 5) ''WorkRequestDTO