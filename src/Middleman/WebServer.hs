{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-
Module      : Middleman.WebServer
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : BSD
Maintainer  : kalhuage@cs.ucla.edu
Stability   : experimental
Description : A simple web frontend
-}
module Middleman.WebServer (webserver, HasTemplates (..)) where

-- rio
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Text.Lazy       as TL
import RIO.Directory
import RIO.Text as Text hiding (index)

-- scotty
import           Web.Scotty.Trans

-- dhall
import qualified Dhall

-- middleman
import           Import              hiding ((<.>), index, Index)

import Middleman.DTO ()
import Middleman.Server.Control
import Middleman.Server.Model (HasSqlPool, GroupQuery(..))
import qualified Middleman.Server.Model as DB

class HasTemplates env where
  templatesL :: Lens' env FilePath

type Action env =
  (HasSqlPool env, HasLogFunc env, HasTemplates env)
  => ActionT TL.Text (RIO env) ()

-- | The webserver of the api
webserver ::
  (HasLogFunc env, HasSqlPool env, HasTemplates env)
  => ScottyT TL.Text (RIO env) ()
webserver = do
  get "/" $ index
  get "/index.html" $ index

data ProgressBar = ProgressBar
  { pclass :: Text
  , pprogress :: Double
  } deriving (Show, Generic)

instance Dhall.Inject ProgressBar

data Index = Index
  { total :: Natural
  , done :: Natural
  , progress :: [ ProgressBar ]
  } deriving (Show, Generic)

instance Dhall.Inject Index

index ::
  Action env
index = do
  DB.JobSummary a s t f w <- lift $ jobSummary (DB.JobQuery Nothing)
  let
    total = fromIntegral (a + s + t + f + w)
    done = fromIntegral (s + f + t)
    progress =
      [ ProgressBar "bg-danger"
        ( fromIntegral f / fromIntegral total * 100)
      , ProgressBar "bg-danger progress-bar-striped"
        ( fromIntegral t / fromIntegral total * 100)
      , ProgressBar "bg-success"
        ( fromIntegral s / fromIntegral total * 100)
      , ProgressBar "bg-success progress-bar-striped"
        ( fromIntegral a / fromIntegral total * 100)
      ]

  serveFile "index.dhall" Dhall.auto $ \r ->
    r (Index {..})


-- * Utils
serveFile ::
  (ScottyError e, MonadIO m, MonadReader env m, HasTemplates env)
  => FilePath
  -> Dhall.Type r
  -> (r -> Text)
  -> ActionT e m ()
serveFile fp tp f = do
  r <- lift $ fromFile fp tp
  htmlText $ f r

htmlText :: (ScottyError e, Monad m) => Text -> ActionT e m ()
htmlText txt = do
  setHeader "Content-Type" "text/html"
  raw . BL.fromStrict $ encodeUtf8 txt

fromFile ::
  (MonadIO m, MonadReader env m, HasTemplates env)
  => FilePath
  -> Dhall.Type a
  -> m a
fromFile fp tp = do
  templates <- view templatesL
  liftIO . withCurrentDirectory templates $ do
    Dhall.inputFrom fp tp =<< readFileUtf8 fp
