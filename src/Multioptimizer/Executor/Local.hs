{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Multioptimizer.Executor.Local where

import qualified Control.Concurrent.MSemN2 as Sem
import Control.Concurrent.STM (atomically, newTVarIO, readTVarIO, readTVar,
                               modifyTVar', TVar, writeTVar,
                               swapTVar, retry)
import Control.Exception (bracket_)
import Control.Monad (forM_, when, unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.IORef (newIORef)
import qualified Data.Vector.Unboxed as U
import Data.Random (runRVarTWith)
import Data.Random.Lift (lift)
import Data.Random.Source.IO ()
import Data.Random.Source.PureMT (newPureMT, pureMT)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word64)
import GHC.Conc (registerDelay, ThreadId, forkIO, myThreadId, killThread)
import Multioptimizer.Backend.Internal (Backend(..))
import Multioptimizer.Util.Pareto
import Multioptimizer.Internal
import System.IO (hSetBuffering, stdout, BufferMode(..))

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
  -- If provided, must be used with 'numThreads' == 1.
  numThreads :: Word,
  -- ^ Number of threads to use to simultaneously run searches on. If greater
  -- than 1, your provided evaluation function will be called simultaneously
  -- by multiple threads.
  verbose :: Bool,
  -- ^ If True, logs metrics to stdout and caught errors to stderr
  objBound :: Maybe (U.Vector Double)
  -- ^ Optional bound on the objective vector. This should be a min bound on
  -- the objectives when 'objectiveType' is 'Maximize', and a max bound
  -- otherwise. When provided and 'verbose' is
  -- 'True', this will be used as the reference point to use when logging
  -- hypervolume values after each new solution is computed. If not provided,
  -- hypervolume values will not be logged.
} deriving (Show, Eq)

data SearchResult a = SearchResult {
  resultFront :: Frontier a,
  resultTotalIters :: Word
} deriving (Show)

defaultOptions :: Options
defaultOptions = Options
  { objectiveType   = Maximize
  , timeLimitMillis = 1000
  , maxIters        = Nothing
  , maxSolutions    = 100
  , randomSeed      = Nothing
  , numThreads      = 1
  , verbose         = True
  , objBound        = Nothing
  }

logHyperVolume :: Options -> Frontier a -> IO ()
logHyperVolume Options {..} front = mapM_ (logIfVerbose . show)
  $ fmap (flip hypervolume front) objBound
  where logIfVerbose x = when verbose $ putStrLn x

registerThreadId :: TVar (Set ThreadId) -> IO ()
registerThreadId s = do
  tid <- myThreadId
  atomically $ modifyTVar' s (Set.insert tid)

unregisterThreadId :: TVar (Set ThreadId) -> IO ()
unregisterThreadId s = do
  tid <- myThreadId
  atomically $ modifyTVar' s (Set.delete tid)

runSearch
  :: Options
  -> Opt a
  -> (a -> IO (U.Vector Double))
  -> Backend a
  -> IO (SearchResult a)
runSearch opts@Options {..} o objFunction (Backend sample update) = do
  hSetBuffering stdout LineBuffering
  randSource <- case randomSeed of
    Nothing -> do
      mt <- liftIO newPureMT
      liftIO (newIORef mt)
    Just s -> liftIO $ newIORef $ pureMT s
  -- searchState is a tuple of frontier and search state
  searchState     <- newTVarIO (mempty, mempty)
  -- tracks how many iterations have been done so far
  itersDone       <- newTVarIO 0
  -- our time limit signal
  done            <- registerDelay (fromIntegral timeLimitMillis * 1000)
  -- set of currently active worker thread ids
  workerThreadIds <- newTVarIO mempty
  -- limits number of simultaneous workers
  workerSem       <- Sem.new numThreads
  void $ forkIO $ spawnLoop workerThreadIds
                            searchState
                            itersDone
                            done
                            randSource
                            workerSem
  resultWaiter done searchState itersDone workerThreadIds
 where
  spawnLoop workerThreadIds searchState itersDone done randSource workerSem =
    do
      alreadyDone <- readTVarIO done
      unless alreadyDone $ do
        Sem.wait   workerSem 1
        Sem.signal workerSem 1
        void $ forkIO $ worker workerThreadIds
                               searchState
                               itersDone
                               done
                               randSource
                               workerSem
        spawnLoop workerThreadIds
                  searchState
                  itersDone
                  done
                  randSource
                  workerSem

  worker workerThreadIds searchState itersDone done randSource workerSem =
    bracket_ (registerThreadId workerThreadIds)
             (unregisterThreadId workerThreadIds)
      $ Sem.with workerSem             1
      $ flip     (runRVarTWith liftIO) randSource
      $ do
          (_, t) <- liftIO $ readTVarIO searchState
          res    <- lift $ runMaybeT $ sample o t
          forM_ res $ \(cs, x) -> do
            objs     <- liftIO $ objFunction x
            frontier <- liftIO $ atomically $ do
              (frontier, state) <- readTVar searchState
              let frontier' = insertSized (x, objs) maxSolutions frontier
              swapTVar    searchState (frontier', update cs objs state)
              modifyTVar' itersDone   (+ 1)
              numDone <- readTVar itersDone
              when (Just True == fmap (== numDone) maxIters)
                   (writeTVar done True)
              return frontier'
            -- TODO: these logs could be printed somewhat out of order.
            liftIO $ logHyperVolume opts frontier

  resultWaiter done searchState itersDone workerThreadIds = do
    (frontier, _) <- atomically $ do
      isDone <- readTVar done
      if isDone then readTVar searchState else retry
    numDone <- readTVarIO itersDone
    tids    <- readTVarIO workerThreadIds
    forM_ (Set.toList tids) killThread
    return $ SearchResult frontier numDone
