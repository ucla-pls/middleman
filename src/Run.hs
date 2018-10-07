module Run
  ( run
  , Mode (..)
  , WorkerOptions(..)
  , ServerOptions(..)
  , PushOptions(..)

  , runDevelop
  ) where

import           Import

import           RIO.Process
import           System.Environment

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

runDevelop :: IO ()
runDevelop = do
  let options_ = Options True 3000
  lo <- logOptionsHandle stderr (view optionsVerbose options_)
  pc <- mkDefaultProcessContext
  username <- getEnv "USER"
  withLogFunc (setLogUseLoc False lo) $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options_
          }
    in runRIO app $ run (
      ModeServer $
        ServerOptions
          "user=middleman password=middleman port=5432 connect_timeout=10"
          "ssh://localhost"
          ( "/nix/var/nix/gcroots/per-user/" ++ username )
          "templates"
      )
