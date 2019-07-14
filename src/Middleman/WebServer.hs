{-# LANGUAGE UndecidableInstances #-}
{-
Module      : Middleman.WebServer
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : BSD
Maintainer  : kalhuage@cs.ucla.edu
Stability   : experimental
Description : A simple web frontend
-}
module Middleman.WebServer (webserver) where

-- base
import Data.Monoid
import Text.Printf

-- http-types
import           Network.HTTP.Types.Status

-- text
import qualified Data.Text as Text

-- rio
import RIO.Time
import qualified RIO.Text.Lazy       as TL

-- blaze-html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8

-- Aeson
-- import Data.Aeson.Text as Aeson

-- scotty
import           Web.Scotty.Trans as S

-- middleman
import Import hiding (index, Index, div)

import Nix.Types (OutputPath (..))
import Middleman.DTO (keyToInt)
import Middleman.Server.Control
import Middleman.Server.Model (HasSqlPool)
import qualified Middleman.Server.Model as DB

type Action env =
  (HasSqlPool env, HasLogFunc env)
  => ActionT TL.Text (RIO env) ()

-- | The webserver of the api
webserver ::
  (HasLogFunc env, HasSqlPool env)
  => ScottyT TL.Text (RIO env) ()
webserver = do
  get "/" index
  get "/index.html" index

  get "/groups/:id" $ do
    key <- S.param "id"
    findOrFail (findGroup key) $ \group -> do
      jobs <- lift $ listJobs (mempty { DB.hasGroupId = Just key })
      renderGroup (DB.entityVal group) jobs

  get "/jobs/:id" $ do
    key <- S.param "id"

    findOrFail (findJob key) $ \(DB.Entity _ job) -> do
      findOrFail (findJobDescription (DB.jobDescId job)) $ \jd -> do
        works <- lift $ listWork (mempty { DB.hasJobId = Just key })
        renderJob job (DB.entityVal jd) works

  get "/main.js" $
    file "static/main.js"


renderGroup ::
  (Monad m, ScottyError e)
  => DB.Group
  -> [DB.Entity DB.Job]
  -> ActionT e m ()
renderGroup DB.Group {..} jobs = template $ do
  h3 (H.text groupName <> ": Jobs")

  p "Jobs, green active"

  div ! class_ "list-group" $
    forM_ jobs $ \( DB.Entity jid (DB.Job {..})) -> do
    a ! href (textValue (Text.pack $ "/jobs/" ++ show (keyToInt jid)))
      ! class_ (
      "list-group-item list-group-item-action flex-column align-items-start "
      <> if isJust jobWorkId then " list-group-item-info" else ""
      ) $ do
      H.text (Text.pack $ storeBaseName jobOutput)

renderJob ::
  (Monad m, ScottyError e)
  => DB.Job
  -> DB.JobDescription
  -> [DB.Entity DB.Work]
  -> ActionT e m ()
renderJob DB.Job {..} _ works = template $ do
  h3 (H.text (Text.pack . storeBaseName $ jobOutput) <> ": Works")
  p "Works, green active"
  div ! class_ "list-group" $
    forM_ works $ \( DB.Entity jid (DB.Work {..})) -> do
    a ! href (textValue (Text.pack $ "/work/" ++ show (keyToInt jid)))
      ! class_ (
      "list-group-item list-group-item-action flex-column align-items-start "
      ) $ do
      H.text (Text.pack . show $ workStarted)


template :: (Monad m, ScottyError e) => Html -> ActionT e m ()
template inner =
  blaze website
  where
    website =
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
        ! src "http://code.jquery.com/jquery-3.3.1.min.js"
        ! dataAttribute "integrity"
              "sha256-FgpCb/KJQlLNfOu91ta32o/NMZxltwRo8QtmkMRdAu8="
        ! dataAttribute "crossorigin" "anonymous" $ ""
      script
        ! src "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.3/umd/popper.min.js"
        ! dataAttribute "integrity"
              "sha384-ZMP7rVo3mIykV+2+9J3UJ46jBk0WLaUAdn689aCwoqbBJiSnjAK/l8WvCWPIPm49"
        ! dataAttribute "crossorigin" "anonymous" $ ""
      script
        ! src "https://cdnjs.cloudflare.com/ajax/libs/moment.js/2.22.2/moment.min.js"
        -- ! dataAttribute "integrity"
        --     "sha384-ChfqqxuZUCnJSK3+MXmPNIyE6ZbWh2IMqE241rYiqJxyMiZ6OW/JmZQ5stwEULTy"
        ! dataAttribute "crossorigin" "anonymous" $ ""
      script
        ! src "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.7.2/Chart.min.js"
        -- ! dataAttribute "integrity"
        --       "sha384-ZMP7rVo3mIykV+2+9J3UJ46jBk0WLaUAdn689aCwoqbBJiSnjAK/l8WvCWPIPm49"
        ! dataAttribute "crossorigin" "anonymous" $ ""
      script
        ! src "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/js/bootstrap.min.js"
        ! dataAttribute "integrity"
            "sha384-ChfqqxuZUCnJSK3+MXmPNIyE6ZbWh2IMqE241rYiqJxyMiZ6OW/JmZQ5stwEULTy"
        ! dataAttribute "crossorigin" "anonymous" $ ""
      script ! src "/main.js?q=1" $ ""
      script "onstart()"

index :: Action env
index = do

  groups <- lift $ listGroupDetails (DB.GroupQuery Nothing)

  now <- getCurrentTime
  tz <- getCurrentTimeZone
  let
    anhourago = addUTCTime (-3600 ) now
    adayago = addUTCTime (-3600 * 24) now

  workers <- lift $ listWorkerDetails adayago (DB.WorkerQuery Nothing)

  let DB.JobSummary {..} = DB.sumJobSummary $ Import.map DB.groupDJobSummary groups

  let Sum pace = foldMap
       (foldMap (\(s, _) -> if s > anhourago then Sum 1 else Sum 0)
        . DB.workerDCompletedJobs) workers

  let
    total = jobSFailed + jobSActive + jobSTimeout + jobSWaiting + jobSSuccess
    done = jobSFailed + jobSSuccess + jobSTimeout
    remaining :: Double = (fromIntegral (jobSWaiting + jobSActive) / pace) * 3600
    estdone = addUTCTime (realToFrac remaining) now

  template $ do
    h3 "Overview!"
    div ! class_ "alert alert-secondary" $ do
      h4 ( "Active Jobs " <> "[" <> toHtml done <> "/" <> toHtml total <> "]" )
      p ( "Currently, on the server we have run "
          <> toHtml done <> " out of "
          <> toHtml total <> " jobs." )

      canvas ! A.id "overview-chart" ! width "400" ! height "200" $ mempty

      -- jobSummaryProgress (js)

      when (pace > 0.1) $
        div ! class_ "d-flex w-100 justify-content-between" $ do
        small (toHtml pace <> " per hour")
        small $ (toHtml $ formatTime
               defaultTimeLocale "%F %H:%M"
               (utcToZonedTime tz estdone))
        small (toHtml (printf "%.2f" $ remaining / 3600 :: String) <> " hours")

      script $
        "function onstart () {renderOverview(\"overview-chart\");}"
    hr

    h3 "Groups"

    div ! class_ "list-group" $
      forM_ groups $ \( DB.GroupDetails {..}) -> do
      a ! href (textValue (Text.pack $ "groups/" ++ show (keyToInt groupDId)))
        ! class_ "list-group-item list-group-item-action flex-column align-items-start" $ do
        div ! class_ "d-flex w-100 justify-content-between" $ do
          h5 ( toHtml $ groupDName )
          small $ "timeout : " <> toHtml (groupDTimeout)
        jobSummaryProgress groupDJobSummary

    hr

    h3 "Workers"

    div ! class_ "list-group" $
      forM_ workers $ \(DB.WorkerDetails {..}) -> do
      a ! class_ "list-group-item list-group-item-action flex-column align-items-start" $ do
        div ! class_ "d-flex w-100 justify-content-between" $ do
          h5 ( toHtml $ workerDName )
          small $ toHtml workerDActiveJobs <> " active jobs"
        p $
          "Completed " <> toHtml (length workerDCompletedJobs )
            <> " jobs in the last 24 hours."

  where
    jobSummaryProgress DB.JobSummary {..} = do
      let total = jobSFailed + jobSActive + jobSTimeout + jobSWaiting + jobSSuccess
      progressBar
        [ ("bg-danger", fromIntegral jobSFailed / fromIntegral total * 100)
        , ("bg-danger progress-bar-striped", fromIntegral jobSTimeout / fromIntegral total * 100)
        , ("bg-success", fromIntegral jobSSuccess / fromIntegral total * 100)
        , ("bg-success progress-bar-striped", fromIntegral jobSActive / fromIntegral total * 100)
        ]

progressBar :: [(Text, Double)] -> Html
progressBar bars =
  div ! class_ "progress" $ do
  forM_ bars $ \(cls, _progress) ->
    div
    ! class_ ("progress-bar " <> toValue cls)
    ! role "progress"
    ! A.style ("width: " <> toValue _progress <> "%")
    ! dataAttribute "aria-valuemin" (toValue (0 :: Double))
    ! dataAttribute "aria-valuenow" (toValue _progress)
    ! dataAttribute "aria-valuemax" (toValue (100 ::Double)) $ mempty

-- | Render some Blaze Html
--
blaze :: (ScottyError e, Monad m) => Html -> ActionT e m ()
blaze h = do
  S.setHeader "Content-Type" "text/html"
  raw ( renderHtml h )

findOrFail ::
  (Monad m, ScottyError e)
  => m (Maybe a)
  -> (a -> ActionT e m ())
  -> ActionT e m ()
findOrFail m fm=
  lift m >>= \case
    Just r -> fm r
    Nothing -> status notFound404
