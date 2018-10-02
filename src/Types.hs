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
  -- Add other app-specific configuration information here
  }


data LocalStore = LocalStore
  { _storeGCRoot :: !String
  }

data OptionsWithApp a = OptionsWithApp
  { innerApp :: !App
  , extraOptions :: !a
  }

makeClassy ''Options
makeClassy ''LocalStore

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
