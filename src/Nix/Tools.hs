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

-- process
import System.Process (interruptProcessGroupOf)

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

-- middleman
import Nix.Types

-- * Copy to the store

copyToStore ::
  (HasLogFunc env
  , HasProcessContext env
  , MonadReader env m
  , MonadIO m
  , InStore a)
  => Store
  -> [a] -- ^ Paths
  -> m ()
copyToStore store fps = do
  ec <- proc
    "nix"
    ( ["copy", "--to", (toStoreUrl store)]
      ++ (map inStore fps)
    ) $ runProcessLogErrors
  when (ec /= ExitSuccess) $
    throwIO $ CouldNotCopyToStore store (map inStore fps)

readDerivationOutput ::
  (HasLogFunc env, HasProcessContext env, MonadReader env m, MonadIO m)
  => Derivation
  -> m OutputPath
readDerivationOutput p = do
  let path = inStore p
  x :: Maybe Value <-
    proc "nix" ["show-derivation", path] $ readProcessJSON
  let output =
        x ^? traverse . key (Text.pack path)
           . key "outputs" . key "out" . key "path" . _JSON
  case output of
    Just filepath -> return $ fromStore filepath
    Nothing -> throwIO $ InvalidDerivation path

realizeDerivation ::
  ( HasLogFunc env
  , HasProcessContext env
  , MonadReader env m
  , MonadUnliftIO m )
  => Derivation -- ^ Derivation
  -> FilePath -- ^ Root
  -> m (Either BL.ByteString [OutputPath])
realizeDerivation drv root = do
  result <- proc "nix-store"
    [ "-j", "1"
    , "-Q", "-r", inStore drv
    , "--add-root", root
    , "--indirect" ]
    $ readProcessLines
  case result of
    Right fps -> do
      paths <- mapM canonicalizePath fps
      return . Right . map fromStore $ paths
    Left msg ->
      return ( Left msg )


class HasGCRoot env where
  gcRootL :: Lens' env FilePath

removeGCRoot ::
  (HasGCRoot env, MonadReader env m, MonadIO m)
  => FilePath
  -> m ()
removeGCRoot name = do
  gcroot <- view gcRootL
  void . liftIO . tryIO $ removeFile (gcroot </> name)

ensureGCRoot ::
  (HasGCRoot env, MonadReader env m, MonadIO m, InStore a)
  => a
  -> FilePath
  -> m ()
ensureGCRoot a name = do
  gcroot <- view gcRootL
  liftIO $ forceCreateSymbolicLink (inStore a) (gcroot </> name)
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
  l <- liftIO $ do
    handleIO (throwIO . InvalidGCRoot file . show ) $ do
      readSymbolicLink file

  linkExits <- liftIO $ doesPathExist l
  when (not linkExits) $ do
    throwIO $ InvalidGCRoot file ("Link does not exits: " ++ l)

-- * Exception

data NixException
  = CouldNotCopyToStore !Store ![FilePath]
  | InvalidDerivation !FilePath
  | InvalidGCRoot !FilePath String
  deriving (Typeable)

instance Exception NixException
deriving instance (Show NixException)

-- * Util

runProcessLogErrors ::
  (MonadReader env m, HasLogFunc env, MonadIO m)
  => ProcessConfig stdin stdout stderr
  -> m ExitCode
runProcessLogErrors pc = do
  res <- safeReadProcess pc
  case res of
    (ec, _, x) -> do
      case decodeUtf8' . BL.toStrict $ x of
        Right txt ->
          forM_ (Text.lines txt) $ \line -> do
            logError $ "[stderr]: " <> display line
        Left err ->
          logError $ "Failed reading the output of failed call: " <> displayShow err
      return ec


safeReadProcess ::
  (MonadIO m)
  => ProcessConfig stdin stdout stderr
  -> m (ExitCode, BL.ByteString, BL.ByteString)
safeReadProcess pc =
  liftIO $
    withProcess
    ( setStderr byteStringOutput . setStdout byteStringOutput . setCreateGroup True $ pc)
    handleProcess
  where
    handleProcess p =
      onException
      ( atomically $ (,,) <$> waitExitCodeSTM p <*> getStdout p <*> getStderr p)
      ( liftIO $ interruptProcessGroupOf (unsafeProcessHandle p) )

readProcessLines ::
  (MonadReader env m, MonadUnliftIO m, HasLogFunc env)
  => ProcessConfig stdin stdout stderrIgnored
  -> m (Either BL.ByteString [String])
readProcessLines pc = do
  res <- safeReadProcess pc
  case res of
    (ExitSuccess, decodeUtf8' . BL.toStrict -> Right txt, _) ->
      return . Right . RIO.map Text.unpack $ Text.lines txt
    (ec, _, x) -> do
      logError $ "Process failed with " <> displayShow ec
      case decodeUtf8' . BL.toStrict $ x of
        Right txt ->
          forM_ (Text.lines txt) $ \line -> do
            logError $ "[stderr]: " <> display line
        Left err ->
          logError $ "Failed reading the output of failed call: " <> displayShow err
      return (Left x)


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
