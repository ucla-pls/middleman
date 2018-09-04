module ServerAccess
  ( HasServerAccess (..)
  , module Network.Wreq
  , get
  , put
  , post

  , fullUrl
  )
where


import RIO hiding (to, Lens', view, (^.))
import Control.Lens

import qualified RIO.ByteString.Lazy as BL

import Network.Wreq hiding (post, put, get)
import qualified Network.Wreq as WR
import qualified Network.Wreq.Types as WR

class HasServerAccess env where
  serverPort :: Lens' env Int
  serverName :: Lens' env String


fullUrl ::
  HasServerAccess env
  => String
  -> Getter env String
fullUrl path =
  to (\a -> "http://" ++ a ^. serverName ++ ":" ++ show (a ^. serverPort) ++ "/" ++ path)

post ::
  (MonadReader env m, MonadIO m, HasServerAccess env, WR.Postable a)
  => String -> a -> m (WR.Response BL.ByteString)
post path p = do
  url <- view (fullUrl path)
  liftIO $ WR.post url p

put ::
  (MonadReader env m, MonadIO m, HasServerAccess env, WR.Putable a)
  => String -> a -> m (WR.Response BL.ByteString)
put path p = do
  url <- view (fullUrl path)
  liftIO $ WR.put url p

get ::
  (MonadReader env m, MonadIO m, HasServerAccess env)
  => String -> m (WR.Response BL.ByteString)
get path = do
  url <- view (fullUrl path)
  liftIO $ WR.get url
