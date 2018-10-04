{-
Module      : Network.HTTP.Client.Helper
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : BSD
Maintainer  : kalhuage@cs.ucla.edu
Stability   : experimental
Description : A small library for describing a http client.

* Goals

- Simple but powerfull DSL for describing http requests to
  servers.

-}
module Network.HTTP.Client.Helper where

-- rio
import RIO
import RIO.Text
import RIO.List as List
import qualified RIO.ByteString.Lazy as BL

-- http-client
import qualified Network.HTTP.Client as H

-- http-types
import Network.HTTP.Types.Status

-- aeson
import Data.Aeson

data Server = Server
  { serverUrl :: Text
  , serverPort :: Int
  } deriving (Show, Eq)

class HasServerAccess env where
  managerL :: Lens' env H.Manager
  serverL :: Lens' env Server

data Request b a = Request
  { path :: Path
  , query :: Query
  , formatter :: b -> BL.ByteString
  , handler :: H.Response BL.ByteString -> IO a
  }

runRequest ::
  (MonadReader env m, HasServerAccess env, MonadIO m)
  => ByteString -> Request b a -> b -> m a
runRequest method req b = do
  server <- view serverL
  manager <- view managerL
  let hreq = H.defaultRequest
        { H.method = method
        , H.host = encodeUtf8 (serverUrl server)
        , H.port = serverPort server
        , H.path = encodeUtf8 . toStringPath $ path req
        , H.queryString =
            encodeUtf8 ( foldMap (\(name, q) -> name <> "=" <> q) (query req) )
        , H.requestBody =
          H.RequestBodyLBS (formatter req b)
        }
  liftIO (
    handler req =<<
      ( H.httpLbs hreq manager `catch` (throwIO . HttpException))
    )

newtype Path = Path { toStringPath :: Text }
type Query = [(Text, Text)]

instance IsString Path where
  fromString path =
    -- TODO: Do check here
    Path (pack path)


class Writeable w where
  writeParam :: w -> Text

instance Writeable Text where
  writeParam = id

instance Writeable Int64 where
  writeParam = pack . show

value :: Writeable a => a -> Path
value = Path . writeParam


infixl 2 </>
(</>) :: Path -> Path -> Path
(</>) p1 p2 =
  Path ( toStringPath p1 <> "/" <> toStringPath p2)

infix 4 =.
(=.) :: Writeable a => Text -> a -> Query
(=.) a = (:[]) . (a, ) . writeParam

infix 1 <?>
(<?>) ::
  Path -> Query
  -> Request BL.ByteString (H.Response BL.ByteString)
(<?>) =
  request


-- * Request

request :: Path -> Query -> Request BL.ByteString (H.Response BL.ByteString)
request path query = Request
  { path = path
  , query = query
  , formatter = id
  , handler = return
  }

expect :: [ Status ] -> Request b a -> Request b a
expect sts req = req
  { handler = \res -> do
      when (not ((H.responseStatus res) `List.elem` sts)) .
        throwIO $ UnexpectedStatus (H.responseStatus res) (H.responseBody res)
      handler req res
  }

expectOk :: Request b a -> Request b a
expectOk = expect [ ok200 ]

expectCreated :: Request b a -> Request b a
expectCreated = expect [ created201 ]

expectAccepted :: Request b a -> Request b a
expectAccepted = expect [ accepted202 ]

expectNoContent :: Request b a -> Request b (Maybe a)
expectNoContent req = expect [ ok200, noContent204 ] $ req
  { handler = \res -> do
      if H.responseStatus res == noContent204
        then return Nothing
        else Just <$> handler req res
  }

json ::
  (FromJSON a, ToJSON b)
  => Request BL.ByteString c
  -> Request b a
json req = req
  { formatter = formatter req . encode
  , handler = \res -> do
      _ <- handler req res
      case eitherDecode (H.responseBody res) of
        Left msg -> throwIO $ BadResponseFormat msg
        Right a -> return a
  }

-- * Methods

get ::
  (MonadReader env m, MonadIO m, HasServerAccess env)
  => Request () a -> m a
get req = do
  runRequest "GET" req ()

put ::
  (MonadReader env m, MonadIO m, HasServerAccess env)
  => b -> Request b a -> m a
put b req = do
  runRequest "PUT" req b

post ::
  (MonadReader env m, MonadIO m, HasServerAccess env)
  => b -> Request b a -> m a
post b req = do
  runRequest "POST" req b

delete ::
  (MonadReader env m, MonadIO m, HasServerAccess env)
  => Request b a -> b -> m a
delete req = do
  runRequest "DELETE" req

-- *

data HttpClientException
  = HttpException !H.HttpException
  | BadResponseFormat !String
  | UnexpectedStatus !Status !BL.ByteString
  deriving (Show, Typeable)

instance Exception HttpClientException
