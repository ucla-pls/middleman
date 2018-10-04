{-
Module      : Middleman.Server.Exception
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : BSD
Maintainer  : kalhuage@cs.ucla.edu
Stability   : experimental
Description : The exceptions of the server
-}
module Middleman.Server.Exception
where

-- base
import Prelude
import Data.Typeable
import Control.Monad

-- UnliftIO
import UnliftIO

-- middleman
import Nix.Tools as Nix

data MiddlemanServerException
  = DatabaseException MiddlemanDatabaseException
  | NixException Nix.NixException
  | SomeOtherException SomeException
  | InvalidInput String
  deriving (Show, Typeable)

instance Exception MiddlemanServerException

data MiddlemanDatabaseException where
  ItemNotFoundException :: forall e. Show e => e -> MiddlemanDatabaseException
  ItemAlreadyExists :: forall e. Show e => e -> MiddlemanDatabaseException

deriving instance (Typeable MiddlemanDatabaseException)
deriving instance (Show MiddlemanDatabaseException)

instance Exception MiddlemanDatabaseException where
  toException = toException . DatabaseException
  fromException = fromException >=> \case
    DatabaseException e -> return e
    _ -> Nothing

-- * Helpers

handleNix ::
  (MonadUnliftIO m)
  => m a
  -> m a
handleNix m =
  m `catch` (throwIO . NixException)

orFail ::
  (MonadIO m, Exception e)
  => m (Maybe a)
  -> e
  -> m a
orFail m e =
  m >>= maybe (throwIO e) return

orFailWith ::
  (MonadIO m, Exception e')
  => m (Either e a)
  -> (e -> m e')
  -> m a
orFailWith m err =
  m >>= either (err >=> throwIO) return
