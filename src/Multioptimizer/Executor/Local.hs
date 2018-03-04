{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Multioptimizer.Executor.Local where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.IORef (newIORef)
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
  randomSeed :: Maybe Word64
  -- ^ Random seed to use for reproducibility.
  -- If 'Nothing', uses system randomness.
} deriving (Show, Eq)

defaultOptions :: Options
defaultOptions = Options
  { objectiveType = Maximize
  , timeLimitMillis   = 1000
  , maxIters          = Nothing
  , maxSolutions      = 100
  , randomSeed        = Nothing
  }

runSearch :: Options
          -> Opt a
          -> (a -> IO (U.Vector Double))
          -> Backend a
          -> IO (Frontier a)
runSearch Options{..} o objFunction (Backend sample) = do
  startTime  <- liftIO currMillis
  randSource <- case randomSeed of
    Nothing -> do
      mt <- liftIO newPureMT
      liftIO (newIORef mt)
    Just s -> liftIO $ newIORef $ pureMT s
  runRVarTWith liftIO (go mempty mempty startTime 0) randSource
 where
  currMillis = (`div` 1000000) . toNanoSecs <$> getTime Monotonic
  go frontier t startTime !iters = do
    currTime <- liftIO currMillis
    let outOfTime  = currTime - startTime > fromIntegral timeLimitMillis
    let outOfIters = (== iters) <$> maxIters
    case (outOfTime, outOfIters) of
      (True, _        ) -> return frontier
      (_   , Just True) -> return frontier
      _                 -> do
        res <- runMaybeT $ sample o objFunction t
        case res of
          Nothing         -> go frontier t startTime (iters + 1)
          Just (t', x, objs) -> go (insertSized (x, objs) maxSolutions frontier)
                                   t'
                                   startTime
                                   (iters + 1)
