{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Multioptimizer.Executor.Local where

import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.STM (atomically, newTVarIO, newTBQueueIO, readTVarIO,
                               modifyTVar', writeTBQueue, tryReadTBQueue,
                               flushTBQueue)
import Control.Monad (forever, replicateM, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Foldable (foldl')
import Data.IORef (newIORef)
import Data.Monoid ((<>))
import qualified Data.Vector.Unboxed as U
import Data.Random (runRVarTWith)
import Data.Random.Source.IO ()
import Data.Random.Source.PureMT (newPureMT, pureMT)
import Data.Word (Word64)
import Multioptimizer.Backend.Internal (Backend(..))
import Multioptimizer.Util.Pareto
import Multioptimizer.Internal
import System.Clock (getTime, Clock(Monotonic), toNanoSecs)

-- | Specifies whether the objectives in an objectives vector should be
-- maximized or minimized. Currently, only maximization problems are supported.
data ObjectiveType = Maximize
  deriving (Show, Eq)

data Options = Options {
  objectiveType :: ObjectiveType,
  -- ^ Whether the objectives should be 'Maximize'd or 'Minimize'd. We do not
  -- yet support mixing maximization and minimization objectives in one problem.
  timeLimitMillis :: Word,
  -- ^ Maximum time to search
  maxIters :: Maybe Word,
  -- ^ Maximum number of iterations to run. One iteration corresponds to the
  -- evaluation of one candidate solution.
  maxSolutions :: Word,
  -- ^ Maximum number of solutions to return. We attempt to return solutions
  -- which are spread across a wide range of the Pareto front. Does not limit
  -- running time, just memory usage. If the number of solutions found during
  -- the search exceeds this number, we remove the most crowded ones to stay
  -- below this number.
  randomSeed :: Maybe Word64,
  -- ^ Random seed to use for reproducibility.
  -- If 'Nothing', uses system randomness.
  numThreads :: Word
} deriving (Show, Eq)

data SearchResult a = SearchResult {
  resultFront :: Frontier a,
  resultTotalIters :: Word
} deriving (Show)

defaultOptions :: Options
defaultOptions = Options
  { objectiveType = Maximize
  , timeLimitMillis   = 1000
  , maxIters          = Nothing
  , maxSolutions      = 100
  , randomSeed        = Nothing
  , numThreads        = 1
  }

runSearch :: Options
          -> Opt a
          -> (a -> IO (U.Vector Double))
          -> Backend a
          -> IO (SearchResult a)
runSearch Options{..} o objFunction (Backend sample) = do
  startTime  <- liftIO currMillis
  randSource <- case randomSeed of
    Nothing -> do
      mt <- liftIO newPureMT
      liftIO (newIORef mt)
    Just s -> liftIO $ newIORef $ pureMT s
  sharedState <- newTVarIO mempty
  queue <- newTBQueueIO (fromIntegral $ 10*numThreads)
  workers <- replicateM (fromIntegral numThreads)
                        (async (worker sharedState queue randSource))
  runRVarTWith liftIO
               (consumer workers sharedState queue mempty startTime 0)
               randSource

 where

  stopCondition :: Bool -> Maybe Bool -> Bool
  stopCondition True _ = True
  stopCondition _ (Just True) = True
  stopCondition _ _ = False

  currMillis = (`div` 1000000) . toNanoSecs <$> getTime Monotonic

  worker sharedState queue randSource =
    flip (runRVarTWith liftIO) randSource $ forever $ do
      t <- liftIO $ readTVarIO sharedState
      res <- runMaybeT $ sample o objFunction t
      forM_ res $ \tuple@(t', _, objs) ->
        objs `seq` t' `seq` liftIO $ atomically $ writeTBQueue queue tuple

  -- TODO: at first a producer/consumer made sense, but now it seems like
  -- overkill -- the producers could atomically swap all their results directly
  -- into the shared state. Leaving this as producer/consumer for now just in
  -- case it becomes necessary for some unforeseen reason. If not, remove.
  consumer workers sharedState queue frontier startTime !iters = do
    currTime <- liftIO currMillis
    let outOfTime  = currTime - startTime > fromIntegral timeLimitMillis
    let outOfIters = (== iters) <$> maxIters
    if stopCondition outOfTime outOfIters
      then do
        liftIO $ forM_ workers cancel
        rest <- liftIO $ atomically $ flushTBQueue queue
        let resultFront = foldl' (\f (_,x,objs) ->
                                  insertSized (x,objs) maxSolutions f)
                                 frontier
                                 rest
        let resultTotalIters = iters + (fromIntegral $ length rest)
        return SearchResult{..}
      else do
        res <- liftIO $ atomically $ tryReadTBQueue queue
        case res of
          Just (t,x,objs) -> do
            let frontier' = insertSized (x,objs) maxSolutions frontier
            liftIO $ atomically $ modifyTVar' sharedState (<> t)
            consumer workers sharedState queue frontier' startTime (iters + 1)
          Nothing ->
            consumer workers sharedState queue frontier startTime iters
