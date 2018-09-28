module WebServer where

import           Import                    hiding ((<.>))

-- base
import qualified Data.Map as Map

-- persit
import qualified Database.Persist.Sql as DB
import qualified Database.Esqueleto as E

-- rio
import qualified RIO.ByteString.Lazy            as BL
import qualified RIO.Text.Lazy            as TL
import qualified RIO.Text as Text
import RIO.Process
import RIO.Directory

-- scotty
import           Web.Scotty.Trans

-- dhall
import qualified Dhall

-- middleman
import Model
import Data.Success

-- | The webserver of the api
webserver ::
  (HasSqlPool env)
  => ScottyT TL.Text (RIO env) ()
webserver = do
  get "/" $ do
    frontpage
  get "/index.html" $ do
    frontpage


data ActiveJobs = ActiveJobs
  { succeded :: Natural
  , failed :: Natural
  , running :: Natural
  , total :: Natural
  , psucceded :: Double
  , pfailed :: Double
  , prunning :: Double
  } deriving (Generic, Show)

instance Dhall.Inject ActiveJobs

data FrontPage = FrontPage
  { activeJobs :: ActiveJobs
  , workers :: [MWorker]
  } deriving (Generic, Show)

instance Dhall.Inject FrontPage

data MWorker = MWorker
  { hostname :: Text
  , workerActiveJobs :: Natural
  } deriving (Generic, Show)

instance Dhall.Inject MWorker


frontpage :: (ScottyError e, HasSqlPool env) => ActionT e (RIO env) ()
frontpage = do
  (total, succeded, failed, running) <- lift . runDB $ do
    total <- DB.count ([] :: [DB.Filter Job])
    jobs :: [(E.Value (Maybe Success), E.Value (Int))] <-
      E.select $ E.from $ \(job `E.InnerJoin` work) -> do
        E.on (job E.^. JobWorkId E.==. E.just (work E.^. WorkId))
        E.groupBy ( work E.^. WorkSuccess)
        return (work E.^. WorkSuccess, E.countRows)

    let sucCount = foldMap (\(E.Value x, E.Value y) -> Map.singleton x y) jobs

    return
     ( total
     , Map.findWithDefault 0 (Just Succeded) sucCount
     , Map.findWithDefault 0 (Just Failed)   sucCount
     , Map.findWithDefault 0 Nothing         sucCount
     )

  workers <- lift . runDB $ do
    E.select $ E.from $ \(worker `E.InnerJoin` work ) -> do
      E.on (worker E.^. WorkerId E.==. work E.^. WorkWorkerId)
      E.where_ (E.isNothing $ work E.^. WorkCompleted )
      E.groupBy (worker E.^. WorkerId)
      return (worker, E.countRows)

  let
    aj = ActiveJobs
         (fromIntegral succeded)
         (fromIntegral failed)
         (fromIntegral running)
         (fromIntegral total)
         (fromIntegral succeded / fromIntegral total * 100)
         (fromIntegral failed / fromIntegral total * 100)
         (fromIntegral running / fromIntegral total * 100)

    mworkers :: [MWorker]
    mworkers =
      map (\(worker, E.Value count) ->
              (MWorker (Text.pack . workerHostname . DB.entityVal $ worker ) count)
              ) workers

  serveFile "frontpage.dhall" Dhall.auto $ \f ->
    f (FrontPage aj mworkers)


templateHtml :: (ScottyError e, MonadIO m) => Text -> ActionT e m ()
templateHtml txt = do
  t <- liftIO . template $ txt
  htmlText t

serveFile :: (ScottyError e, MonadIO m) => FilePath -> Dhall.Type r -> (r -> Text) -> ActionT e m ()
serveFile fp tp f = do
  r <- liftIO $ fromFile fp tp
  htmlText $ f r

htmlText :: (ScottyError e, Monad m) => Text -> ActionT e m ()
htmlText txt = do
  setHeader "Content-Type" "text/html"
  raw . BL.fromStrict $ encodeUtf8 txt


fromFile :: FilePath -> Dhall.Type a -> IO a
fromFile fp tp =
  withCurrentDirectory "templates" $ do
    Dhall.inputFrom fp tp =<< readFileUtf8 fp

template :: Text -> IO Text
template txt = do
  withCurrentDirectory "templates" $ do
    let fn = "index.dhall"
    fs <- readFileUtf8 fn
    temp <- Dhall.inputFrom fn Dhall.auto fs :: IO (Text -> Text)
    return $ temp txt
