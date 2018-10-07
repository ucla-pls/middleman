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
module Middleman.WebServer (webserver, HasTemplates(..)) where

-- rio
-- import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Text.Lazy       as TL
-- import RIO.Directory
-- import RIO.Text as Text hiding (index, span)

-- blaze-html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8

-- scotty
import           Web.Scotty.Trans as S

-- middleman
import           Import              hiding ((<.>), index, Index, div, id)

import Middleman.DTO ()
import Middleman.Server.Control
import Middleman.Server.Model (HasSqlPool)
import qualified Middleman.Server.Model as DB

class HasTemplates env where
  templatesL :: Lens' env FilePath

type Action env =
  (HasSqlPool env, HasLogFunc env)
  => ActionT TL.Text (RIO env) ()

-- | The webserver of the api
webserver ::
  (HasLogFunc env, HasSqlPool env)
  => ScottyT TL.Text (RIO env) ()
webserver = do
  get "/" $ index
  get "/index.html" $ index


template :: (Monad m, ScottyError e) => Html -> ActionT e m ()
template inner = do
  blaze website
  where
    website = do
      docTypeHtml $ do
        head $ do
          meta ! A.charset "utf-8"
          meta ! A.name "viewport"
               ! A.content "width=device-width, initial-scale1, shrink-to-fit=no"
          H.link
            ! A.href "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css"
            ! A.rel "stylesheet"

        H.body $ do
          div ! class_ "container" $ do
            h1 ! A.style "margin-top: 1rem;" $
              "Middleman"
            p "The middleman make sure that the derivations gets build."
            hr
            inner
            scripts

    scripts = do
      script
        ! src "https://code.jquery.com/jquery-3.3.1.slim.min.js"
        ! dataAttribute "integrity"
              "sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo"
        ! dataAttribute "crossorigin" "anonymous" $ ""
      script
        ! src "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.3/umd/popper.min.js"
        ! dataAttribute "integrity"
              "sha384-ZMP7rVo3mIykV+2+9J3UJ46jBk0WLaUAdn689aCwoqbBJiSnjAK/l8WvCWPIPm49"
        ! dataAttribute "crossorigin" "anonymous" $ ""
      script
        ! src "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/js/bootstrap.min.js"
        ! dataAttribute "integrity"
            "sha384-ChfqqxuZUCnJSK3+MXmPNIyE6ZbWh2IMqE241rYiqJxyMiZ6OW/JmZQ5stwEULTy"
        ! dataAttribute "crossorigin" "anonymous" $ ""

index :: Action env
index = do
  _ <- lift $ jobSummary (DB.JobQuery Nothing)
  template (H.span "hello")


-- | Render some Blaze Html
--
blaze :: (ScottyError e, Monad m) => Html -> ActionT e m ()
blaze h = do
  S.setHeader "Content-Type" "text/html"
  raw ( renderHtml h )

-- -- | Render a generic builder
-- --
-- builder :: Builder -> ActionT e ()
-- builder = MS.modify . setContent

-- setContent :: Builder -> Response -> Response
-- setContent b (ResponseBuilder s h _) = ResponseBuilder s h b
-- setContent b (ResponseFile s h _ _)  = ResponseBuilder s h b
-- setContent b (ResponseSource s h _)  = ResponseBuilder s h b



-- data ProgressBar = ProgressBar
--   { pclass :: Text
--   , pprogress :: Double
--   } deriving (Show, Generic)

-- instance Dhall.Inject ProgressBar

-- data Index = Index
--   { total :: Natural
--   , done :: Natural
--   , progress :: [ ProgressBar ]
--   } deriving (Show, Generic)

-- instance Dhall.Inject Index

-- index ::
--   Action env
-- index = do
--   DB.JobSummary a s t f w <- lift $ jobSummary (DB.JobQuery Nothing)
--   let
--     total = fromIntegral (a + s + t + f + w)
--     done = fromIntegral (s + f + t)
--     progress =
--       [ ProgressBar "bg-danger"
--         ( fromIntegral f / fromIntegral total * 100)
--       , ProgressBar "bg-danger progress-bar-striped"
--         ( fromIntegral t / fromIntegral total * 100)
--       , ProgressBar "bg-success"
--         ( fromIntegral s / fromIntegral total * 100)
--       , ProgressBar "bg-success progress-bar-striped"
--         ( fromIntegral a / fromIntegral total * 100)
--       ]

--   serveFile "index.dhall" Dhall.auto $ \r ->
--     r (Index {..})


-- -- * Utils
-- serveFile ::
--   (ScottyError e, MonadIO m, MonadReader env m, HasTemplates env)
--   => FilePath
--   -> Dhall.Type r
--   -> (r -> Text)
--   -> ActionT e m ()
-- serveFile fp tp f = do
--   r <- lift $ fromFile fp tp
--   htmlText $ f r

-- htmlText :: (ScottyError e, Monad m) => Text -> ActionT e m ()
-- htmlText txt = do
--   setHeader "Content-Type" "text/html"
--   raw . BL.fromStrict $ encodeUtf8 txt

-- fromFile ::
--   (MonadIO m, MonadReader env m, HasTemplates env)
--   => FilePath
--   -> Dhall.Type a
--   -> m a
-- fromFile fp tp = do
--   templates <- view templatesL
--   liftIO . withCurrentDirectory templates $ do
--     Dhall.inputFrom fp tp =<< readFileUtf8 fp
