{-# LANGUAGE TemplateHaskell #-}
module Client where


-- rio
import RIO.FilePath
import RIO.Text as Text
import RIO.List as List

-- http-client
import Network.HTTP.Client

-- middleman
import Import
import Middleman.Client as Client
import Middleman.DTO
import qualified Nix.Tools as Nix

data PushOptions = PushOptions
  { _copsServer :: !Server
  , _copsGroup :: !Text
  , _copsDerivations :: ![FilePath]
  }

makeClassy ''PushOptions

type PushApp = OptionsWithApp (PushOptions, Manager)

instance HasPushOptions PushApp where
  pushOptions = extraOptionsL . _1

instance HasServerAccess PushApp where
  serverL = pushOptions . copsServer
  managerL = extraOptionsL . _2


push :: PushOptions -> RIO App ()
push ops = do
  manager <- liftIO $ newManager defaultManagerSettings
  ask >>= \app -> runRIO (OptionsWithApp app (ops, manager)) pushApp

pushApp = do
  derivations <- List.map Nix.fromStorePath <$> view copsDerivations
  groupName <- view copsGroup

  info <- Client.getInfo

  groupId <- Client.getGroupWithName groupName >>= \case
    Just grp ->
      return (entityKey grp)
    Nothing ->
      fail $ "Group with name " ++ show groupName ++ "does not exist."

  forM_ derivations $ \drv -> do
    jkey <- entityKey <$> postJobDescription
      (JobDescription (Nix.derivationName drv) groupId )

    Nix.copyToStore (infoStoreUrl info) [Nix.inStore drv]

    publishJobDescription jkey

-- client ::
--   ClientOptions
--   -> RIO App ()
-- client ops = ask >>= \app -> runRIO (OptionsWithApp app ops) $ do
--   forM_ (ops ^.. copsDrvs . folded . to (\d -> (d, takeBaseName d))) $ \(drv, name) -> do

--     r <- post "api/jobs" . toJSON $ NewJobDTO name (ops^.copsGroup)

--     case (r ^. responseStatus . statusCode, r ^? responseBody . key "id" . _JSON) of
--       (201, Just (i :: Int)) -> do
--         logInfo $ "Created new job @ " <> displayShow i

--         logInfo $ "Copying the derivation to the store"
--         copyToStore (ops ^. copsStoreUrl) [drv]
--         logInfo $ "Done"

--         r' <- put ("api/jobs/" ++ show i ++ "/publish") (toJSON True)
--         let s = r' ^. responseStatus
--         case s ^. statusCode of
--           200 ->
--             logInfo $ "Successfully published derivation"
--           sc ->
--             logError $ "Error publishing the derivation expected 200, got: "
--               <> display sc <> ": "
--               <> displayBytesUtf8 (s ^. statusMessage)

--       (x, a) ->
--         logError $ "Got " <> displayShow a <> " from the server and " <> display x <> ": " <>
--           displayBytesUtf8 (r^.responseStatus.statusMessage)
