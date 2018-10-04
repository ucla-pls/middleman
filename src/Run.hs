module Run
  ( run
  , Mode (..)
  , WorkerOptions(..)
  , ServerOptions(..)
  , PushOptions(..)
  ) where

import           Import

import           Middleman.Push
import           Middleman.Server
import           Middleman.Worker


data Mode
  = ModeServer !ServerOptions
  | ModePush !PushOptions
  | ModeWorker !WorkerOptions

run :: Mode -> RIO App ()
run mode = do
  case mode of
    ModeServer sops ->
      server sops
    ModeWorker wops ->
      worker wops
    ModePush cops ->
      push cops
