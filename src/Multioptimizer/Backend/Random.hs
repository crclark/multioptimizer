{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Multioptimizer.Backend.Random (
  sample
, runOpt
, randomSearch
) where

import Multioptimizer.Internal
import Multioptimizer.Util.Pareto

import Data.Functor.Identity (Identity)
import Data.IORef (newIORef)
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Operational (ProgramViewT(Return,(:>>=)), view)
import Data.Random (sampleState, RVarT, uniformT, runRVarTWith)
import Data.Random.Source.PureMT (pureMT, newPureMT)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word
import System.Clock (getTime, Clock(Monotonic), toNanoSecs)

-- | Create a single random sample of the given generator.
-- We return 'Maybe a' because a generator can fail to produce a value.
-- Consider @uniformChoice []@.
sample
  :: Opt a
  -> Word64
          -- ^ Random seed
  -> Maybe a
sample a seed =
  let randAction = runMaybeT $ runOpt @Identity a
  in  fst $ sampleState randAction (pureMT seed)

runOpt :: Monad m => Opt a -> MaybeT (RVarT m) a
runOpt (Opt o) = case view o of
  Return x                  -> return x
  (UniformChoice xs :>>= m) -> if V.null xs
    then mzero
    else do
      ix <- lift $ Data.Random.uniformT 0 (V.length xs - 1)
      runOpt $ Opt $ m $ xs V.! ix

data RandomOpts = RandomOpts {
  timeLimitMillis :: Word,
  maxIters :: Maybe Word,
  maxSolutions :: Word,
  randomSeed :: Maybe Word64
} deriving Show

-- TODO: lots of repetition between this and MCTS equivalent.
-- We will eventually need to add a swappable execution layer that abstracts
-- this loop (and that can handle parallel and distributed execution).

randomSearch
  :: RandomOpts -> Opt a -> (a -> IO (U.Vector Double)) -> IO (Frontier a)
randomSearch RandomOpts {..} o eval = do
  startTime  <- liftIO currMillis
  randSource <- case randomSeed of
    Nothing -> do
      mt <- liftIO newPureMT
      liftIO (newIORef mt)
    Just s -> liftIO $ newIORef $ pureMT s
  runRVarTWith liftIO (go startTime 0 mempty) randSource
 where
  currMillis = (`div` 1000000) . toNanoSecs <$> getTime Monotonic
  go startTime !iters front = do
    currTime <- liftIO currMillis
    let outOfTime  = currTime - startTime > fromIntegral timeLimitMillis
    let outOfIters = (== iters) <$> maxIters
    case (outOfTime, outOfIters) of
      (True, _        ) -> return front
      (_   , Just True) -> return front
      _                 -> do
        res <- runMaybeT $ runOpt o
        case res of
          Nothing -> go startTime (iters + 1) front
          Just x  -> do
            objs <- liftIO (eval x)
            let front' = shrinkToSize maxSolutions $ insert (x, objs) front
            go startTime (iters + 1) front'
