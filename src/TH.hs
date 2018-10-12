module TH where

import RIO
import RIO.Char

import           Data.Aeson
import           Data.Aeson.Casing

def :: Int -> Options
def c =
  ( defaultOptions
    { fieldLabelModifier = snakeCase . drop c
    , constructorTagModifier = map toLower
    }
  )
