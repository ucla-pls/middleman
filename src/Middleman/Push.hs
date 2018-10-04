{-# LANGUAGE TemplateHaskell #-}
{-
Module      : Middleman.Push
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : BSD
Maintainer  : kalhuage@cs.ucla.edu
Stability   : experimental
Description : The push functionality

Pushes nix derivations to the server.
-}
module Middleman.Push where

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
import qualified Nix.Types as Nix

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

pushApp :: RIO (OptionsWithApp (PushOptions, Manager)) ()
pushApp = do
  derivations <- List.map Nix.fromStore <$> view copsDerivations
  groupName <- view copsGroup

  info <- Client.getInfo
  groupId <- fetchGroupName groupName

  let descriptions =
        List.map (flip JobDescription groupId)
        derivations

  logDebug "Ensure job descriptions exist on the server..."

  results <- postJobDescriptions descriptions

  uploads <-
    filterM removeConflictingDerivation
    ( List.zipWith compareResults results descriptions )

  if (List.null uploads)
    then logInfo "No jobs needs uploading..."
    else do
      logInfo $
        "Copy " <> display (List.length uploads) <> " jobs to the server..."

      Nix.copyToStore (infoStoreUrl info)
        . List.map (\(_, _, desc) -> jobDescriptionDerivation desc)
        $ uploads

      forM_ uploads $ \(jkey, _, desc) -> do
        ejob <- publishJobDescription jkey
        logInfo $
          "Done uploading "
          <> displayShow (jobDescriptionDerivation desc)
          <> " : " <> display jkey
          <> " as " <> display (entityKey ejob)
  where
    removeConflictingDerivation (key, isSame, desc) = do
      let JobDescription drv groupId = desc
      if not isSame
        then do
        logDebug $ "Derivation "
          <> displayShow drv
          <> " exist with other group "
          <> display groupId
        return False
        else do
        getJobWithDescription key >>= \case
          Just job -> do
            logDebug $ "Derivation "
              <> displayShow drv
              <> " already have a job "
              <> display (entityKey job)
            return False
          Nothing ->
            return True

    compareResults (Entity key val) desc =
      (key, val == desc, desc)

    fetchGroupName groupName = do
      Client.getGroupWithName groupName >>= \case
        Just grp ->
          return (entityKey grp)
        Nothing ->
          fail $ "Group with name " ++ show groupName ++ "does not exist."
