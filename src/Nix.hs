{-# LANGUAGE TemplateHaskell #-}
module Nix where

import Import hiding (view, (<.>))

import TH

import Data.Aeson.TH
import qualified Data.Aeson as J
import Data.Aeson.Lens

import RIO.Process
import RIO.Text as Text
import RIO.FilePath
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Map as Map

data NixDerivation = NixDerivation
  { drvOutputs :: Map String (Map String FilePath)
  } deriving (Show, Generic)

deriveJSON (def 4) ''NixDerivation

newtype Derivation = Derivation
  { drvName :: String
  } deriving (Show)

derivationInStore :: Derivation -> FilePath
derivationInStore drv =  "/nix/store" </> ( drvName drv ) <.> "drv"

copyToStore ::
  (HasLogFunc env, HasProcessContext env)
  => String
  -> [FilePath] -- ^ Paths
  -> RIO env ()
copyToStore store fps = do
  ec <- proc "nix" (["copy", "--to", store] ++ fps) $ runProcess
  when (ec /= ExitSuccess) $
    throwString $ "Could not transfer " ++ show fps ++ " to "++ show store

copyFromStore ::
  (HasLogFunc env, HasProcessContext env)
  => String
  -> [FilePath] -- ^ Paths
  -> RIO env Bool
copyFromStore store fps = do
  ec <- proc "nix" (["copy", "--from", store] ++ fps) $ runProcess
  return $ ec == ExitSuccess

importNar ::
  (HasLogFunc env, HasProcessContext env)
  => BL.ByteString
  -> RIO env (Maybe [FilePath])
importNar bs = do
  proc "nix-store"
    ["--import"]
    $ readProcessLines
    . setStdin (byteStringInput bs)

exportNar ::
  (HasLogFunc env, HasProcessContext env)
  => [FilePath]
  -> RIO env (Maybe BL.ByteString)
exportNar fps = do
  (ec, bc) <- proc "nix-store"
    (["--export"] ++ fps)
    $ readProcessStdout
  return $ case ec of
    ExitSuccess -> Just bc
    _ -> Nothing

realizeDrv ::
  (HasLogFunc env, HasProcessContext env)
  => Derivation -- ^ Derivation
  -> FilePath -- ^ Root
  -> RIO env (Maybe [FilePath])
realizeDrv drv root = do
  proc "nix-store"
    ["-r", derivationInStore drv, "--add-root", root, "--indirect"]
    readProcessLines

readDerivation ::
  (HasLogFunc env, HasProcessContext env, MonadReader env m, MonadIO m)
  => FilePath
  -> m (Maybe NixDerivation)
readDerivation p = do
  x :: Maybe (Map FilePath NixDerivation) <-
    proc "nix" ["show-derivation", p] $ readProcessJSON
  return . join $ Map.lookup p <$> x

readDerivationOutput ::
  (HasLogFunc env, HasProcessContext env, MonadReader env m, MonadIO m)
  => FilePath
  -> m (Maybe FilePath)
readDerivationOutput p = do
  x :: Maybe J.Value <-
    proc "nix" ["show-derivation", p] $ readProcessJSON
  return $ x ^? _Just . key (Text.pack p) . key "outputs". key "out" . key "path" . _JSON

readProcessLines ::
  MonadIO m
  => ProcessConfig stdin stdout stderrIgnored
  -> m (Maybe [String])
readProcessLines p = do
  (ec, bs) <- readProcessStdout p
  return $
    case (ec, decodeUtf8'. BL.toStrict $ bs) of
      (ExitSuccess, Right txt) ->
        Just (txt ^.. to Text.lines . traverse . to unpack)
      _ ->
        Nothing

readProcessJSON ::
  (MonadIO m, J.FromJSON a)
  => ProcessConfig stdin stdout stderrIgnored
  -> m (Maybe a)
readProcessJSON p = do
  (ec, bs) <- readProcessStdout p
  return $
    case (ec, J.decode $ bs) of
      (ExitSuccess, Just e) -> e
      _ ->
        Nothing
