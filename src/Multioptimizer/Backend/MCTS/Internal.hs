{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

module Multioptimizer.Backend.MCTS.Internal where

import Control.Error (fromMaybe, hoistMaybe)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Operational (ProgramViewT(Return,(:>>=)), view)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Function (on)
import Data.IORef (newIORef)
import Data.Ord (comparing)
import Data.Semigroup (Semigroup(..))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.IntMap.Strict as IM
import Data.Random (RVarT, runRVarTWith, uniformT)
import Data.Random.Source.IO ()
import Data.Random.Source.PureMT (newPureMT, pureMT)
import Data.Traversable (forM)
import Data.Word (Word64)
import Safe.Foldable (maximumByMay, minimumByMay)
import Multioptimizer.Backend.Internal (Backend(..))
import Multioptimizer.Backend.Random (runOpt)
import Multioptimizer.Util.Pareto
import Multioptimizer.Internal
import GHC.Exts(IsList(..))

-- | Options for one run of Monte Carlo tree search. See 'defaultOpts' for
-- defaults.
data MCTSOpts = MCTSOpts {
  maxSolutions :: Word,
  -- ^ Maximum number of solutions to store in each branch of the tree.
  explorationFactor :: Double
  -- ^ weight to give to exploration. Must be greater than zero. @1 / sqrt 2@ is
  -- a theoretically advisable number that is used in 'defaultOpts'. See A
  -- Survey of Monte Carlo Tree Search Methods by Browne et al., 2012.
} deriving (Show, Eq)

-- | A set of reasonable default options.
defaultOpts :: MCTSOpts
defaultOpts = MCTSOpts
  { maxSolutions = 100
  , explorationFactor = 1.0 / sqrt 2.0
  }

mcts :: MCTSOpts -> Backend a
mcts opts = Backend (mctsAddSample opts)

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
insertSubResult _ _ (ReturnLeaf _) = error "impossible case in insertSubResult"
insertSubResult _ (subtree, DiscreteChosen{..}, objs) Unvisited =
  DiscreteBranch { _treeFrontier = insert ((), objs) mempty
                 , numVisits = 1
                 , expandedChoices = IM.singleton discreteChosenIndex subtree
                 }

-- | Represents the current state of the Monte Carlo search tree.
-- Branches represent choices that have been expanded. Leaves are unexpanded
-- parts of the tree that have not been explored yet.
data Tree = DiscreteBranch {
  _treeFrontier :: Frontier (),
  numVisits :: Word,
  -- ^ Number of times we have passed through this choice. Note this is distinct
  -- from @size expandedChoices@, because we could take the same choice more
  -- than once.
  expandedChoices :: IM.IntMap Tree
  -- ^ branches to choices that have already been expanded
}
 | ReturnLeaf {numVisits :: Word}
 | Unvisited
  deriving Show

-- TODO: many impossible cases come from merging trees of different shapes,
-- or the Opt a not matching the tree generated from it. Think of ways to use
-- the types to make these impossible.

instance Semigroup Tree where
  (DiscreteBranch tf1 n1 ec1) <> (DiscreteBranch tf2 n2 ec2) =
    DiscreteBranch (tf1 <> tf2) (n1 + n2) (IM.unionWith (<>) ec1 ec2)
  (ReturnLeaf n1) <> (ReturnLeaf n2) =
    ReturnLeaf (n1 + n2)
  (ReturnLeaf n1) <> (DiscreteBranch tf2 n2 ec2) =
    DiscreteBranch tf2 (n1 + n2) ec2
  (DiscreteBranch tf1 n1 ec1) <> (ReturnLeaf n2) =
    DiscreteBranch tf1 (n1 + n2) ec1
  Unvisited <> t = t
  t <> Unvisited = t

instance Monoid Tree where
  mempty = Unvisited
  mappend = (<>)

treeFrontier :: Tree -> Frontier ()
treeFrontier DiscreteBranch{..} = _treeFrontier
treeFrontier _ = mempty

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
  $ fromMaybe 0 $ minimumByMay (comparing f) ([0 .. maxIndex] :: [Word])
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
  case V.length xs of
    0 -> mzero
    l -> do
      let refpoint =
            foldr1 (U.zipWith min)
              $ map snd
              $ concatMap (toList . treeFrontier . snd)
              $ toList (expandedChoices t)
      let hyperVols =
            IM.map (hypervolume refpoint . treeFrontier) (expandedChoices t)
      let numChildVisits ix = case IM.lookup ix (expandedChoices t) of
            Nothing       -> 0.0
            Just (ReturnLeaf n) -> fromIntegral n
            Just b        -> fromIntegral $ numVisits b
      let discreteChosenIndex = fromIntegral $ uct (numVisits t)
                                                   explorationFactor
                                                   numChildVisits
                                                   hyperVols
                                                   (fromIntegral $ l - 1)
      let chosenChoice = xs V.! discreteChosenIndex
      return DiscreteChosen {..}
choose MCTSOpts{..} Unvisited (UniformChoice xs) = do
  discreteChosenIndex <- fromIntegral <$> lift (uniformT 0 (V.length xs - 1))
  let chosenChoice = xs V.! discreteChosenIndex
  return DiscreteChosen {..}
choose _ (ReturnLeaf _) _ = error "impossible case in choose"

defaultStrategy :: Opt a -> MaybeT (RVarT IO) a
defaultStrategy = runOpt

-- | Initialize an empty tree for a new problem.
initTree :: Opt a -> Tree
initTree = const mempty

-- | Initialize a new subtree from a single random rollout.
initDiscreteSubtree :: Opt a -> U.Vector Double -> Tree
initDiscreteSubtree (Opt o) objs = case view o of
  (Return _) -> ReturnLeaf 1
  _          -> DiscreteBranch
    { _treeFrontier   = insert ((), objs) mempty
    , numVisits       = 1
    , expandedChoices = IM.empty
    }

-- TODO: we currently don't remember the route through the tree once we get
-- to the defaultStrategy (random simulation). This is a holdover from MCTS's
-- standard pseudocode, meant for 2-player games, where large chunks of the
-- tree eventually become irrelevant because of actions by the opponent.
-- These limitations don't apply here, though.

-- If we did add the entire random simulation to the tree, the current
-- Tree representation would make it appear that we had entered "choice mode"
-- all the way down to the return statement along that path through the tree,
-- which isn't actually true. The problem is that the Leaf constructor currently
-- has two meanings: 1. corresponding to a return statement and 2. corresponding
-- to the switchover to the default strategy.

-- | one iteration of MCTS for a given tree. Adds one sample.
-- Returns the subtree corresponding to the choices taken to construct the new
-- sample, the new sample, and its evaluation. Does *not* return the full
-- updated tree! The caller is responsible for 'mappend'ing the returned tree
-- to the input tree if desired.
mctsAddSample
  :: MCTSOpts
  -> Opt a
  -> (a -> IO (U.Vector Double))
  -> Tree
  -> MaybeT (RVarT IO) (Tree, a, U.Vector Double)
mctsAddSample opts@MCTSOpts {..} (Opt o) eval t = case (view o, t) of
  (Return x, ReturnLeaf _) -> do
    objs <- liftIO $ eval x
    return (ReturnLeaf 1, x, objs)
  (Return x, Unvisited) -> do
    objs <- liftIO $ eval x
    return (ReturnLeaf 1, x, objs)
  (_, ReturnLeaf _) -> error "impossible ReturnLeaf case in mctsAddSample"
  (Return _, _     ) -> error "impossible case in mctsAddSample"
  (_       , Unvisited) -> do
    sampled <- defaultStrategy (Opt o)
    objs    <- liftIO $ eval sampled
    return (initDiscreteSubtree (Opt o) objs, sampled, objs)
  (choice :>>= m, b@DiscreteBranch {..}) -> do
    chosen <- choose opts b choice
    -- TODO: standardize naming of intermediate vars. Some are newX, others x'
    case chosen of
      dc@(DiscreteChosen ix c) -> do
        let nextStep = Opt (m c)
        let subtree  = IM.findWithDefault Unvisited ix expandedChoices
        (newSubtree, sampled, objs) <- mctsAddSample opts nextStep eval subtree
        let b' = insertSubResult opts (newSubtree, dc, objs) mempty
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
  -> Maybe Word64
  -> IO (Maybe (a, U.Vector Double))
nestedMCTS MCTSOpts {..} o eval depth randomSeed = do
  randSource <- case randomSeed of
    Nothing -> do
      mt <- liftIO newPureMT
      liftIO (newIORef mt)
    Just s -> liftIO $ newIORef $ pureMT s
  let comp = runMaybeT $ nested o eval depth
  runRVarTWith liftIO comp randSource

