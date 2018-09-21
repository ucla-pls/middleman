module Run
  ( run
  , Mode (..)
  , WorkerOptions(..)
  , ServerOptions(..)
  , ClientOptions(..)
  ) where

import           Import

import           Client
import           Server
import           Worker


data Mode
  = ModeServer !ServerOptions
  | ModeClient !ClientOptions
  | ModeWorker !WorkerOptions

run :: Mode -> RIO App ()
run mode = do
  case mode of
    ModeServer sops ->
      server sops
    ModeWorker wops ->
      worker wops
    ModeClient cops ->
      client cops
