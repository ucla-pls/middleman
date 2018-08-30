{-# LANGUAGE TemplateHaskell #-}
module Types where

import RIO hiding (lens, Lens', (^.))
import RIO.Process

import Control.Lens

-- | Command line arguments
data Options = Options
  { _optionsVerbose :: !Bool
  , _optionsPort :: !Int
  }

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  , appMode :: !Mode
  -- Add other app-specific configuration information here
  }

data WorkerOptions = WorkerOptions
  { _wopsHostUrl :: !String
  }

data Mode
  = ModeServer
  | ModeConsumer
  | ModeWorker WorkerOptions

data WorkerApp = WorkerApp
  { wappApp :: !App
  , wappOptions :: !WorkerOptions
  }

makeClassy ''Options
makeClassy ''WorkerOptions

innerApp :: Lens' WorkerApp App
innerApp = lens wappApp (\x y -> x { wappApp = y})

instance HasOptions App where
  options = lens appOptions (\x y -> x { appOptions = y })

instance HasWorkerOptions WorkerApp where
  workerOptions = lens wappOptions (\x y -> x { wappOptions = y })

instance HasOptions WorkerApp where
  options = innerApp . options

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

instance HasLogFunc WorkerApp where
  logFuncL = innerApp . logFuncL

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })

instance HasProcessContext WorkerApp where
  processContextL = innerApp . processContextL
