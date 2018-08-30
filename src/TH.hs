module TH where

import RIO
import RIO.Char

import           Data.Aeson

def :: Int -> Options
def c =
  ( defaultOptions
    { fieldLabelModifier = map toLower . drop c
    , constructorTagModifier = map toLower
    }
  )
