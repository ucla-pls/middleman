{-
Module      : Middleman.Worker
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : BSD
Maintainer  : kalhuage@cs.ucla.edu
Stability   : experimental
Description : The worker of the middleman.

-}
module Middleman.Worker
  where

-- rio
import RIO
import RIO.Process
import qualified RIO.Text as Text
import qualified RIO.ByteString.Lazy as BL

-- UnliftIO
import UnliftIO


-- * Access commands

-- -- | Perform a job with derivation name
-- performJob ::
--   ( HasLogFunc env, HasProcessContext env)
--   => Derivation
--   -- ^ Derivation name
--   -> String
--   -- ^ Expected output path
--   -> RIO env (Maybe FilePath)
-- performJob drv output = do
--   threadId <- myThreadId
--   succ <- realizeDrv drv ("middleman-link/" ++ show threadId)
--   case succ of
--     Just fps ->
--       case fps of
--         [fp] -> do
--           path <- canonicalizePath fp
--           when (path /= "/nix/store" </> output)
--             ( throwIO $ JobExecutedWrong
--               ( "Returned path different: " ++ show path ++ " /= " ++ show output )
--             )
--           return $ Just path
--         _ -> do
--           throwIO . JobExecutedWrong $
--             "Returned more than one path: " ++ show fps
--     Nothing ->
--       return Nothing

-- * Exceptions

data WorkerException
  -- = FailedRequest HttpException
  = BadResponseFormat String
  | JobExecutedWrong String
  | CouldNotGetHostName String
  deriving (Typeable)

instance Show WorkerException where
  show = \case
    -- FailedRequest e ->
    --   "Failed request: " ++ show e
    BadResponseFormat str ->
      "Received bad formatted response from server: " ++ show str
    JobExecutedWrong str ->
      "Job executed wrong:" ++ show str
    CouldNotGetHostName str ->
      "Could not get hostname: " ++ show str

instance Exception WorkerException

-- * Utils

-- | Gets the hostname in the current system using this command
getHostName ::
  (HasProcessContext env, HasLogFunc env, MonadUnliftIO m, MonadReader env m)
  => m Text.Text
getHostName = do
  s <- proc "hostname" [] readProcessStdout_
    `catchAny` (throwIO . CouldNotGetHostName . show)
  case Text.lines <$> Text.decodeUtf8' (BL.toStrict s) of
    Right (hostname:_) -> return hostname
    _ -> throwIO (CouldNotGetHostName "Could not parse output")

-- | Converts any double to an integer
seconds :: Double -> Int
seconds a =
  round (a * 1e6)
