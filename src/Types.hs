{-# LANGUAGE TemplateHaskell #-}
module Types where

import RIO hiding (lens, Lens', (^.))
import RIO.Process

import Control.Lens

import ServerAccess hiding (Options, options)

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
  , _wopsStoreUrl :: !String
  }

data ClientOptions = ClientOptions
  { _copsHostUrl :: !String
  , _copsDrv :: !String
  }

data Mode
  = ModeServer
  | ModeClient !ClientOptions
  | ModeWorker !WorkerOptions

data WorkerApp = WorkerApp
  { wappApp :: !App
  , wappOptions :: !WorkerOptions
  }

data ClientApp = ClientApp
  { cappApp :: !App
  , cappOptions :: !ClientOptions
  }

makeClassy ''Options
makeClassy ''WorkerOptions
makeClassy ''ClientOptions

wInnerApp :: Lens' WorkerApp App
wInnerApp = lens wappApp (\x y -> x { wappApp = y})

cInnerApp :: Lens' ClientApp App
cInnerApp = lens cappApp (\x y -> x { cappApp = y})

instance HasWorkerOptions WorkerApp where
  workerOptions = lens wappOptions (\x y -> x { wappOptions = y })

instance HasClientOptions ClientApp where
  clientOptions = lens cappOptions (\x y -> x { cappOptions = y })

instance HasOptions App where
  options = lens appOptions (\x y -> x { appOptions = y })

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })


instance HasOptions WorkerApp where
  options = wInnerApp . options

instance HasLogFunc WorkerApp where
  logFuncL = wInnerApp . logFuncL

instance HasProcessContext WorkerApp where
  processContextL = wInnerApp . processContextL

instance HasServerAccess WorkerApp where
  serverPort = options . optionsPort
  serverName = workerOptions . wopsHostUrl

instance HasOptions ClientApp where
  options = cInnerApp . options

instance HasLogFunc ClientApp where
  logFuncL = cInnerApp . logFuncL

instance HasProcessContext ClientApp where
  processContextL = cInnerApp . processContextL

instance HasServerAccess ClientApp where
  serverPort = options . optionsPort
  serverName = clientOptions . copsHostUrl
