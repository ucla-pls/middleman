{-
Module      : Nix.Tools
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : BSD
Maintainer  : kalhuage@cs.ucla.edu
Stability   : experimental
Description : Tools for interacting with Nix
-}
module Nix.Tools
  where

-- lens
import Control.Lens ((^?))

-- base
import Data.Typeable
import Text.Show

-- aeson
import Data.Aeson
import Data.Aeson.Lens (_JSON, key)

-- unix
import           System.Posix.Files        (createSymbolicLink, readSymbolicLink)

-- rio
import RIO
import qualified RIO.Text as Text
import RIO.Process
import RIO.FilePath
import RIO.Directory
import qualified RIO.ByteString.Lazy as BL


-- * Derivation

newtype Derivation =
  Derivation { derivationName :: Text.Text }
  deriving (Show, Eq)

inStore ::
  Derivation -> FilePath
inStore drv =
  "/nix/store" </> Text.unpack (derivationName drv) <.> "drv"

-- * Verification of nix.

-- * Copy to the store

-- | The url to the store.
type Store = String

copyToStore ::
  (HasLogFunc env, HasProcessContext env, MonadReader env m, MonadIO m)
  => Store
  -> [FilePath] -- ^ Paths
  -> m ()
copyToStore store fps = do
  ec <- proc "nix" (["copy", "--to", store] ++ fps) $ runSilentProcess
  when (ec /= ExitSuccess) $
    throwIO $ CouldNotCopyToStore store

readDerivationOutput ::
  (HasLogFunc env, HasProcessContext env, MonadReader env m, MonadIO m)
  => Derivation
  -> m FilePath
readDerivationOutput p = do
  let path = inStore p
  x :: Maybe Value <-
    proc "nix" ["show-derivation", path] $ readProcessJSON
  let output =
        x ^? traverse . key (Text.pack path)
           . key "outputs" . key "out" . key "path" . _JSON
  case output of
    Just filepath -> return filepath
    Nothing -> throwIO $ InvalidDerivation path

realizeDerivation ::
  (HasLogFunc env, HasProcessContext env, MonadReader env m, MonadIO m)
  => Derivation -- ^ Derivation
  -> FilePath -- ^ Root
  -> m (Maybe [FilePath])
realizeDerivation drv root = do
  proc "nix-store"
    [ "-j", "1"
    , "-r", inStore drv
    , "--add-root", root
    , "--indirect"
    ]
    readProcessLines


class HasGCRoot env where
  gcRootL :: Lens' env FilePath

removeGCRoot ::
  (HasGCRoot env, MonadReader env m, MonadIO m)
  => FilePath
  -> m ()
removeGCRoot name = do
  gcroot <- view gcRootL
  liftIO $ removeFile (gcroot </> name) `onException` return ()

ensureGCRoot ::
  (HasGCRoot env, MonadReader env m, MonadIO m)
  => FilePath
  -> FilePath
  -> m ()
ensureGCRoot storepath name = do
  gcroot <- view gcRootL
  liftIO $ forceCreateSymbolicLink
    ("/nix/store" </> storepath)
    (gcroot </> name)
  where
    forceCreateSymbolicLink dest src = do
      catchAny (createSymbolicLink dest src) $ \_ -> do
        catchAny ( guard . (== dest) =<< readSymbolicLink src ) $ \_ -> do
          removeFile src
          createSymbolicLink dest src

validateLink ::
  (HasGCRoot env, MonadReader env m, MonadUnliftIO m)
  => FilePath
  -> m ()
validateLink name = do
  gcroot <- view gcRootL
  let file = (gcroot </> name)
  link <- liftIO $ do
    handleIO (throwIO . InvalidGCRoot file . show ) $ do
      readSymbolicLink file

  linkExits <- liftIO $ doesPathExist link
  when (not linkExits) $ do
    throwIO $ InvalidGCRoot file ("Link does not exits: " ++ link)

-- * Exception

data NixException
  = CouldNotCopyToStore !Store
  | InvalidDerivation !FilePath
  | InvalidGCRoot !FilePath String
  deriving (Typeable)

instance Exception NixException
deriving instance (Show NixException)

-- * Util

runSilentProcess ::
  (MonadIO m)
  => ProcessConfig stdin stdout stderrIgnored
  -> m ExitCode
runSilentProcess =
  runProcess
  . setStdout closed
  . setStderr closed
  . setStdin closed

readProcessLines ::
  MonadIO m
  => ProcessConfig stdin stdout stderrIgnored
  -> m (Maybe [String])
readProcessLines p = do
  (ec, bs) <- readProcessStdout p
  return $
    case (ec, decodeUtf8'. BL.toStrict $ bs) of
      (ExitSuccess, Right txt) ->
        Just (RIO.map Text.unpack $ Text.lines txt)
      _ ->
        Nothing

readProcessJSON ::
  (MonadIO m, FromJSON a)
  => ProcessConfig stdin stdout stderrIgnored
  -> m (Maybe a)
readProcessJSON p = do
  (ec, bs) <- readProcessStdout p
  return $
    case (ec, decode $ bs) of
      (ExitSuccess, Just e) -> e
      _ ->
        Nothing
