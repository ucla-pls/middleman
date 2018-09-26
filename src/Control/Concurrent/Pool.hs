module Control.Concurrent.Pool
  ( PoolState
  , PoolSettings (..)
  , Regulator (Regulator)
  , dispatch
  , runPool
  , waitForPool
  ) where

-- base
import  Prelude
import           Control.Monad
import           Control.Concurrent
import           Data.Foldable

-- unliftio
import           UnliftIO
import qualified Data.Set as Set

data PoolSettings m = PoolSettings
  { maxJobs :: !Int
  , regulators :: ![ Regulator m ]
  , regulateEvery :: !Seconds
  }

type Seconds = Rational

-- | A regulator can kill or start new jobs. If the action returns
-- LT, then it will kill active jobs, if it returns GT it will try to
-- add more jobs to the limit of maxJobs.
newtype Regulator m = Regulator { runRegulator :: m Ordering }

data PoolState m = PoolState
  { currentMaxJobs :: !(TVar Int)
  , currentWorkers :: !(TVar (Set.Set (Async ())))
  , currentWaitingJobs :: !(TChan (m ()))
  }


newPoolStateSTM :: PoolSettings m -> STM (PoolState m)
newPoolStateSTM (PoolSettings{..}) = do
  currentMaxJobs <- newTVar maxJobs
  currentWorkers <- newTVar Set.empty
  currentWaitingJobs <- newTChan
  return (PoolState {..})

newPoolState :: MonadUnliftIO m => PoolSettings m -> m (PoolState m)
newPoolState =
  atomically . newPoolStateSTM

cleanupPoolState :: MonadUnliftIO m => (PoolState m) -> m ()
cleanupPoolState ps = do
  workers <- atomically $ readTVar (currentWorkers ps)
  mapM_ uninterruptibleCancel workers

dispatch :: MonadUnliftIO m =>
  (PoolState m)
  -> m ()
  -> m ()
dispatch ps@(PoolState{..}) work = do
  -- Write the job to the queue
  atomically $ do
    writeTChan currentWaitingJobs work

  -- Ensure that request have been processed, if the pool is blocked, this will block
  atomically $ do
    guard =<< isEmptyTChan currentWaitingJobs


waitForPool ::
  MonadUnliftIO m
  => PoolState m
  -> m ()
waitForPool (PoolState {..}) = do
  atomically $ do
    isEmptyTChan currentWaitingJobs >>= guard
    Set.null <$> readTVar currentWorkers >>= guard

runPoolManager ::
  MonadUnliftIO m =>
  PoolState m
  -> m ()
runPoolManager (PoolState {..}) = do
  mv <- newEmptyTMVarIO
  forever $ do
    -- Do we have too many jobs, or too few.
    job <- atomically $ do
      -- Wait for any other thread to start.
      isEmptyTMVar mv >>= guard
      -- Check if there is room for new threads
      workers <- Set.size <$> readTVar currentWorkers
      jobs <- readTVar currentMaxJobs
      case compare workers jobs of
        EQ -> retrySTM
        LT -> Just <$> readTChan currentWaitingJobs
        GT -> return Nothing

    case job of
      Just work -> do
        -- Create a new worker
        w <- mask_ $ asyncWithUnmask $ \restore -> do
          bracket
            ( atomically $ do
                w <- takeTMVar mv
                modifyTVar currentWorkers (Set.insert w)
                return w
            )
            ( atomically . modifyTVar currentWorkers . Set.delete )
            ( const (
                  (restore work)
                    `withException`
                    \(AsyncExceptionWrapper e) -> do
                      case fromException (toException e) of
                        Just RegulationKillException -> do
                          atomically (writeTChan currentWaitingJobs work)
                        Nothing -> return ()
                  )
            )

        -- Start the worker
        atomically ( putTMVar mv w )
      Nothing -> do
        a <- Set.findMin <$> readTVarIO currentWorkers
        cancelWith a RegulationKillException

-- | Create an environment where we can run and kill jobs.
runPool ::
  forall m a. (MonadUnliftIO m)
  => PoolSettings m
  -> (PoolState m -> m a)
  -> m a
runPool poolSettings@(PoolSettings {..}) run = do
  bracket (newPoolState poolSettings) cleanupPoolState $ \ps -> do
    withAsync (runPoolManager ps) $ \_ -> do
        withAsync ( runRegulators ps ) $ \_ ->
          run ps
  where
    runRegulators :: PoolState m -> m ()
    runRegulators (PoolState {..}) = forever $ do
      liftIO $ threadDelay (floor (regulateEvery * 1e6))
      cmp <- fold <$> mapM runRegulator regulators
      case cmp of
        EQ ->
          return ()
        LT -> do
          atomically $ do
            jobs <- readTVar currentMaxJobs
            when (jobs > 1) $ do
              writeTVar currentMaxJobs (jobs - 1)
        GT -> do
          atomically $ do
            jobs <- readTVar currentMaxJobs
            when (jobs < maxJobs) $ do
              writeTVar currentMaxJobs (jobs + 1)


data RegulationKillException =
  RegulationKillException
  deriving (Show, Typeable)

instance Exception RegulationKillException
