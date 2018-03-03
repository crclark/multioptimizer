{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Multioptimizer.Backend.MCTS where

import Control.Error (hoistMaybe)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Operational (ProgramViewT(Return,(:>>=)), view)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.IORef (newIORef)
import Data.Ord (comparing)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.IntMap.Strict as IM
import Data.Random (RVarT, runRVarTWith, uniformT)
import Data.Random.Source.IO ()
import Data.Random.Source.PureMT (newPureMT, pureMT)
import Data.Traversable (forM)
import Data.Word (Word64)
import Safe.Foldable (maximumByMay)
import Multioptimizer.Backend.Random (runOpt)
import Multioptimizer.Util.Pareto
import Multioptimizer.Internal
import GHC.Exts(IsList(..))
import System.Clock (getTime, Clock(Monotonic), toNanoSecs)

-- | Specifies whether the objectives in an objectives vector should be
-- maximized or minimized. Currently, only maximization problems are supported.
data ObjectiveType = Maximize
  deriving (Show, Eq)

-- | Options for one run of Monte Carlo tree search. See 'defaultOpts' for
-- defaults.
data MCTSOpts = MCTSOpts {
  objectiveType :: ObjectiveType,
  -- ^ Whether the objectives should be 'Maximize'd or 'Minimize'd. We do not
  -- yet support mixing maximization and minimization objectives in one problem.
  timeLimitMillis :: Word,
  -- ^ Maximum time to search
  maxIters :: Maybe Word,
  -- ^ Maximum number of iterations to run. One iteration corresponds to the
  -- evaluation of one candidate solution.
  explorationFactor :: Double,
  -- ^ weight to give to exploration. Must be greater than zero. @1 / sqrt 2@ is
  -- a theoretically advisable number that is used in 'defaultOpts'. See A
  -- Survey of Monte Carlo Tree Search Methods by Browne et al., 2012.
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


-- | A set of reasonable default options.
defaultOpts :: MCTSOpts
defaultOpts = MCTSOpts
  { objectiveType = Maximize
  , timeLimitMillis   = 1000
  , maxIters          = Nothing
  , explorationFactor = 1.0 / sqrt 2.0
  , maxSolutions      = 100
  , randomSeed        = Nothing
  }

-- | Represents a chosen branch at a given step of traversing the tree.
-- Contains the intermediate value chosen, the index of that value in the
-- vector of choices, and whether we have chosen this branch before.
data Chosen intermed = DiscreteChosen {
  discreteChosenIndex :: Int,
  chosenChoice :: intermed
} deriving Show

insertSubResult
  :: MCTSOpts
  -> (Tree, Chosen intermed, U.Vector Double)
                -- ^ Subtree, chosen branch, subresult, its evaluation
  -> Tree
                -- ^ current tree
  -> Tree
                -- ^ updated tree
insertSubResult MCTSOpts {..}
                (subtree, DiscreteChosen {..}, objs)
                DiscreteBranch {..}
  = DiscreteBranch
    { _treeFrontier = shrinkToSize maxSolutions $ insert ((), objs) _treeFrontier
    , numVisits = numVisits + 1
    , expandedChoices = IM.insert discreteChosenIndex subtree expandedChoices
    }
insertSubResult _ _ (Leaf _) = error "impossible case in insertSubResult"

data Tree = DiscreteBranch {
  _treeFrontier :: Frontier (),
  numVisits :: Word,
  -- ^ Number of times we have passed through this choice. Note this is distinct
  -- from @size expandedChoices@, because we could take the same choice more
  -- than once.
  expandedChoices :: IM.IntMap Tree
  -- ^ branches to choices that have already been expanded
}
 | Leaf {numVisits :: Word}
  deriving Show

treeFrontier :: Tree -> Frontier ()
treeFrontier (Leaf _) = mempty
treeFrontier t        = _treeFrontier t

-- | Monte Carlo tree search, returning the resulting frontier
mcts :: MCTSOpts -> Opt a -> (a -> IO (U.Vector Double)) -> IO (Frontier a)
mcts opts@MCTSOpts {..} o objFunction = do
  startTime  <- liftIO currMillis
  randSource <- case randomSeed of
    Nothing -> do
      mt <- liftIO newPureMT
      liftIO (newIORef mt)
    Just s -> liftIO $ newIORef $ pureMT s
  runRVarTWith liftIO (go mempty (initTree o) startTime 0) randSource
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
        res <- runMaybeT $ mctsAddSample opts o objFunction t
        case res of
          Nothing         -> go frontier t startTime (iters + 1)
          Just (t', x, objs) -> go (insertSized (x, objs) maxSolutions frontier)
                                   t'
                                   startTime
                                   (iters + 1)

-- TODO: this uct is slightly wrong as-is. The problem is that it's supposed to
-- be using the *average* reward of a given choice, but we're using the
-- current hypervolume. This could be kind of like the average, except that
-- the user's evaluation function could be noisy. In that case, the elitism of
-- our Pareto front means that we always prefer the highest value of the eval
-- function for any given sampled structure. That would seem to be biased.

-- TODO: another way in which it's maybe wrong: it scales the relative rewards
-- by the worst and best alternatives available.

-- TODO: this uct selects unvisited nodes in the order they appear in the
-- input vector. This should be documented somewhere, or, even better, we should
-- have a way for the user to provide a policy for deciding which unvisited
-- node to prefer.

-- | Selects the index of the best child node to visit, using the upper
-- confidence bound for trees (UCT) algorithm.
-- NOTE: not total when the IntMap is empty, but only called when guarded by the
-- size of the map. TODO: Clean up.
uct
  :: Word
    -- ^ treeNumVisits of parent node
  -> Double
    -- ^ exploration factor C_p > 0
  -> (Int -> Double)
    -- ^ Function assigning each child index to the number of times it has
    -- been visited.
  -> IM.IntMap Double
    -- ^ map from child index to hypervolume scores
  -> Word
    -- ^ discreteChoiceMax
  -> Word
    -- ^ index of best child
uct n c_p numVisits hyperVols maxIndex = fromIntegral
  $ minimumBy (comparing f) ([0 .. maxIndex] :: [Word])
 where
  f :: Word -> Maybe Double
  f ix = case IM.lookup (fromIntegral ix) scaledHyperVols of
    Nothing -> Nothing
    Just hypervol ->
      let denom = numVisits (fromIntegral ix)
          numer :: Double
          numer = 2.0 * log (fromIntegral n)
      in  Just $ negate $ hypervol + 2.0 * c_p * sqrt (numer / denom)
      -- note: we negate the standard equation so that when denom = 0,
      -- we return Nothing and minimumBy considers Nothing less than
      -- any other value.
  maxHyperVol     = maximum (map snd (toList hyperVols))
  minHyperVol     = minimum (map snd (toList hyperVols))
  scaledHyperVols = IM.map (\hv -> (hv - minHyperVol)
                                   / (maxHyperVol - minHyperVol))
                           hyperVols

choose
  :: MCTSOpts
  -> Tree
  -> OptInstr intermed
  -> MaybeT (RVarT IO) (Chosen intermed)
choose MCTSOpts {..} t@DiscreteBranch{} (UniformChoice xs) =
  case (IM.size (expandedChoices t), V.length xs) of
    (_, 0) -> mzero
    (0, l) -> do
      discreteChosenIndex <- fromIntegral <$> lift (uniformT 0 (l - 1))
      let chosenChoice = xs V.! discreteChosenIndex
      return DiscreteChosen {..}
    (_, l) -> do
      let refpoint =
            foldr1 (U.zipWith min)
              $ map snd
              $ concatMap (toList . treeFrontier . snd)
              $ toList (expandedChoices t)
      let hyperVols =
            IM.map (hypervolume refpoint . treeFrontier) (expandedChoices t)
      let numChildVisits ix = case IM.lookup ix (expandedChoices t) of
            Nothing       -> 0.0
            Just (Leaf n) -> fromIntegral n
            Just b        -> fromIntegral $ numVisits b
      let discreteChosenIndex = fromIntegral $ uct (numVisits t)
                                                   explorationFactor
                                                   numChildVisits
                                                   hyperVols
                                                   (fromIntegral $ l - 1)
      let chosenChoice = xs V.! discreteChosenIndex
      return DiscreteChosen {..}
choose _ (Leaf _) _ = error "impossible case in choose"

defaultStrategy :: Opt a -> MaybeT (RVarT IO) a
defaultStrategy = runOpt

-- | Initialize an empty tree for a new problem.
initTree :: Opt a -> Tree
initTree (Opt o) = case view o of
  Return _                 -> Leaf {numVisits = 0}
  (UniformChoice _ :>>= _) -> DiscreteBranch
    { _treeFrontier   = mempty
    , numVisits       = 0
    , expandedChoices = IM.empty
    }

-- | Initialize a new subtree from a single random rollout.
initDiscreteSubtree :: Opt a -> U.Vector Double -> Tree
initDiscreteSubtree (Opt o) objs = case view o of
  (Return _) -> Leaf 1
  _          -> DiscreteBranch
    { _treeFrontier   = insert ((), objs) mempty
    , numVisits       = 1
    , expandedChoices = IM.empty
    }

-- | one iteration of MCTS for a given tree. Adds one sample.
-- Returns the updated tree along with the sample that was added to the tree
-- and its evaluation.
mctsAddSample
  :: MCTSOpts
  -> Opt a
  -> (a -> IO (U.Vector Double))
  -> Tree
  -> MaybeT (RVarT IO) (Tree, a, U.Vector Double)
mctsAddSample opts@MCTSOpts {..} (Opt o) eval t = case (view o, t) of
  (Return x, Leaf n) -> do
    objs <- liftIO $ eval x
    return (Leaf (n + 1), x, objs)
  (Return _, _     ) -> error "impossible case in mctsAddSample"
  (_       , Leaf _) -> do
    sampled <- defaultStrategy (Opt o)
    objs    <- liftIO $ eval sampled
    return (initDiscreteSubtree (Opt o) objs, sampled, objs)
  (choice :>>= m, b@DiscreteBranch {..}) -> do
    chosen <- choose opts b choice
    -- TODO: standardize naming of intermediate vars. Some are newX, others x'
    case chosen of
      dc@(DiscreteChosen ix c) -> do
        let nextStep = Opt (m c)
        let subtree  = IM.findWithDefault (Leaf 0) ix expandedChoices
        (newSubtree, sampled, objs) <- mctsAddSample opts nextStep eval subtree
        let b' = insertSubResult opts (newSubtree, dc, objs) b
        return (b', sampled, objs)

-- TODO: this isn't quite right. Nested MCTS records the globally best sequence
-- and always plays the move from that sequence, even if it wasn't found in the
-- current step. Need to add a StateT.

-- | basic nested MCTS, no tree returned.
-- Returns a non-dominated solution if one can be found.
nested
  :: Opt a
  -> (a -> IO (U.Vector Double))
          -- ^ evaluation function
  -> Int
          -- ^ levels before just random sampling
  -> MaybeT (RVarT IO) (a, U.Vector Double)
nested (Opt o) eval l = case (view o, l) of
  (Return x               , _) -> (x,) <$> liftIO (eval x)
  (UniformChoice xs :>>= m, 1) -> do
    let subtrees = V.map (Opt . m) xs
    sampled <- mapM runOpt subtrees
    sampledWObjs <- mapM (\x -> (x,) <$> liftIO (eval x)) sampled
    hoistMaybe $ maximumByMay (domOrdering `on` snd) sampledWObjs
  (UniformChoice xs :>>= m, i) -> do
    subresults <- forM
      xs
      (\x -> do
        (_, score) <- nested (Opt $ m x) eval (i - 1)
        return (x, score)
      )
    bestMove <-
      hoistMaybe $ fst <$> maximumByMay (domOrdering `on` snd) subresults
    nested (Opt (m bestMove)) eval (i - 1)

nestedMCTS
  :: MCTSOpts
  -> Opt a
  -> (a -> IO (U.Vector Double))
  -> Int
  -> IO (Maybe (a, U.Vector Double))
nestedMCTS MCTSOpts {..} o eval depth = do
  randSource <- case randomSeed of
    Nothing -> do
      mt <- liftIO newPureMT
      liftIO (newIORef mt)
    Just s -> liftIO $ newIORef $ pureMT s
  let comp = runMaybeT $ nested o eval depth
  runRVarTWith liftIO comp randSource

