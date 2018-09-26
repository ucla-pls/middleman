module Control.Concurrent.PoolSpec where

import Import
import Test.Hspec
import Test.Hspec.QuickCheck

import Data.Semigroup
import Data.Maybe
import Data.List

import Control.Concurrent.Pool

import UnliftIO

spec :: Spec
spec = do
  describe "runPool" $ do
    it "can run two jobs in parallel" $ do
      runPool (PoolSettings 2 [] 60) $ \ps -> do
        x <- newTVarIO (0 :: Int)
        dnto $ dispatch ps (incr x 1)
        dnto $ dispatch ps (incr x 2)

        incr x 0

        r <- notADeadlock (incr x 3)
        r `shouldBe` Just ()

    it "does not run more than two things i parallel" $ do
      runPool (PoolSettings 2 [] 60) $ \ps -> do
        x <- newTVarIO (0 :: Int)

        dnto $ dispatch ps ( incr x 1 )
        dnto $ dispatch ps ( incr x 2 )

        -- Does not reach this statement, because the other events locks.
        r <- notADeadlock (dispatch ps ( incr x 0 ))
        r `shouldBe` Nothing

    it "can run more than 2 things i parallel, if we let them" $ do
      runPool (PoolSettings 3 [] 60) $ \ps -> do
        x <- newTVarIO (0 :: Int)

        dnto $ dispatch ps ( incr x 1 )
        dnto $ dispatch ps ( incr x 2 )

        -- Does not reach this statement, because the other events locks.
        r <- notADeadlock (dispatch ps ( incr x 0 ))
        r `shouldBe` Just ()

    it "can run more than 2 things i serial" $ do
      runPool (PoolSettings 2 [] 60) $ \ps -> do
        x <- newTVarIO (0 :: Int)

        dnto $ dispatch ps ( incr x 1 )
        dnto $ dispatch ps ( incr x 0 )
        dnto $ dispatch ps ( incr x 2 )
        dnto $ dispatch ps ( incr x 4 )
        dnto $ dispatch ps ( incr x 3 )

    it "can be regulated" $ do
      x <- newTVarIO ([0] :: [Int])
      c <- newTVarIO (0 :: Int)
      let exactly2  = Regulator (compare 2 . head <$> readTVarIO x)
      runPool (PoolSettings 4 [ exactly2 ] 1e-1) $ \ps -> do
        replicateM_ 4 $ do
          dispatch ps $
            ( do
                atomically $ do
                  modifyTVar c (+1)
                  modifyTVar x (\x' -> (head x' + 1) : x')
                threadDelay (floor (1e6 :: Rational))
            ) `finally`
            ( do
                atomically $ do
                  modifyTVar x (\x' -> (head x' - 1) : x')
            )
        waitForPool ps
        things <- reverse <$> readTVarIO x
        other <- readTVarIO c
        (other, things) `shouldBe` (6,[0,1,2,3,4,3,2,1,0,1,2,1,0])


dnto :: IO () -> IO ()
dnto m = do
  x <- timeout (floor (1e5 :: Rational)) m
  x `shouldBe` Just ()

notADeadlock :: IO () -> IO (Maybe ())
notADeadlock m = do
  timeout (floor (1e5 :: Rational)) m

incr :: TVar Int -> Int -> IO ()
incr x n = atomically $ do
  v <- readTVar x
  guard (v >= n)
  writeTVar x (v + 1)
