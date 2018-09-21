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
  -- Add other app-specific configuration information here
  }

data ClientOptions = ClientOptions
  { _copsHostUrl :: !String
  , _copsStoreUrl :: !String
  , _copsDrv :: !String
  }

data ServerOptions = ServerOptions
  { _sopsRunMigration :: !Bool
  , _sopsConnectionString :: !Text
  , _sopsLocalStore :: !LocalStore
  }

data LocalStore = LocalStore
  { _storeGCRoot :: !String
  }


data OptionsWithApp a = OptionsWithApp
  { innerApp :: !App
  , extraOptions :: !a
  }

type ClientApp = OptionsWithApp ClientOptions

makeClassy ''Options
makeClassy ''ClientOptions
makeClassy ''ServerOptions
makeClassy ''LocalStore

instance HasClientOptions ClientApp where
  clientOptions = lens extraOptions (\x y -> x { extraOptions = y })

instance HasOptions App where
  options = lens appOptions (\x y -> x { appOptions = y })

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })

class HasApp env where
  innerAppL :: Lens' env App

instance HasApp (OptionsWithApp hl) where
  innerAppL = lens innerApp (\x y -> x { innerApp = y })

instance HasOptions (OptionsWithApp env) where
  options = innerAppL . options

instance HasLogFunc (OptionsWithApp env) where
  logFuncL = innerAppL . logFuncL

instance HasProcessContext (OptionsWithApp env) where
  processContextL = innerAppL . processContextL

instance HasServerAccess ClientApp where
  serverPort = options . optionsPort
  serverName = clientOptions . copsHostUrl

