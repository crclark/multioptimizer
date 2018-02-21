{-# LANGUAGE RecordWildCards #-}

module Multioptimizer.Backend.MCTS where

import Control.Monad.Operational (ProgramViewT(Return,(:>>=)), view)
import Data.Word
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.Ord (comparing)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.IntMap.Strict as IM
import Data.Random (sampleState, RVar, RVarT, uniformT)
import Data.Foldable (asum)
import Data.Traversable (forM)
import Safe.Foldable (maximumByMay)
import Multioptimizer.Backend.Random (toRVarT)
import Multioptimizer.Util.Pareto
import Multioptimizer.Internal
import GHC.Exts(IsList(..))

data ChoiceInfo a = DiscreteChoice {
  discreteChoiceMax :: Word,
  -- ^ choices are in the range 0,discreteChoiceMax, inclusive
  expandedChoices :: IM.IntMap (Tree a)
  -- ^ branches to choices that have already been expanded
  } deriving Show

data Chosen intermed = DiscreteChosen {
  discreteChosenIndex :: Int,
  chosenChoice :: intermed,
  isChosenNew :: Bool
  -- ^ True iff we have chosen to expand a branch we haven't explored before
} deriving Show

updateChoiceInfo :: Chosen intermed -> ChoiceInfo a -> ChoiceInfo a
updateChoiceInfo = undefined

data Tree a = Branch {
  treeFrontier :: Frontier a,
  treeNumVisits :: Word,
  treeChoiceInfo :: ChoiceInfo a
}
 | Leaf
  deriving Show

-- | Monte Carlo tree search, returning the resulting tree
mcts :: Opt a -> Tree a -> RVarT IO (Tree a)
mcts (Opt o) t = undefined

uct :: Word
    -- ^ treeNumVisits of parent node
    -> Double
    -- ^ exploration factor C_p > 0
    -> (Word -> Double)
    -- ^ Function assigning each child index to the number of times it has
    -- been visited.
    -> V.Vector Double
    -- ^ Scaled hypervolume scores
    -> Word
    -- ^ index of best child
uct n c_p numVisits scores =
  -- TODO: not total!
  fst $ maximumBy (comparing f) (zip [0..] (toList scores))
  where f (ix,score) = let denom = numVisits ix
                           numer :: Double
                           numer = 2.0 * log (fromIntegral n)
                           in score + 2.0 * sqrt (numer/denom)

-- TODO: this is ignoring the possibility that xs is empty!
choose :: ChoiceInfo a -> OptInstr intermed -> RVarT IO (Chosen intermed)
choose DiscreteChoice{..} (UniformChoice xs) =
  case IM.size expandedChoices of
    0 -> do
      discreteChosenIndex <- fromIntegral <$> uniformT 0 discreteChoiceMax
      let chosenChoice = (xs V.! discreteChosenIndex)
      let isChosenNew = True
      return DiscreteChosen{..}
    _ -> do
      let refpoint = foldr1 (U.zipWith min) $
                     map snd $
                     concatMap toList $
                     map (treeFrontier . snd) $ toList expandedChoices
      let hyperVols = IM.map (hypervolume refpoint . treeFrontier)
                             expandedChoices
      let maxHyperVol = maximum (map snd (toList hyperVols))
      let scaledHyperVols = IM.map (/maxHyperVol) hyperVols
      let numChildVisits ix = case IM.lookup ix expandedChoices of
                               Nothing -> 0.0
                               Just t -> fromIntegral $ treeNumVisits t
      return undefined

defaultStrategy :: Opt a -> RVarT IO a
defaultStrategy = toRVarT

-- | one iteration of MCTS for a given tree. Adds one sample.
-- Returns the updated tree along with the sample that was added to the tree
-- and its evaluation.
mctsAddSample :: Opt a
                 -> (a -> U.Vector Double)
                 -> Tree a
                 -> RVarT IO (Tree a, a, U.Vector Double)
mctsAddSample (Opt o) eval t = case (view o, t) of
  (Return x, Leaf) -> return (Leaf, x, eval x)
  (Return _, _) -> error "impossible case 1 in mctsAddSample"
  (_ :>>= _, Leaf) -> error "impossible case 2 in mctsAddSample"
  (choice :>>= m, b@Branch{..}) -> do
    chosen <- choose treeChoiceInfo choice
    case isChosenNew chosen of
      True -> do
        sampled <- defaultStrategy (Opt (m (chosenChoice chosen)))
        let objs = eval sampled
        let b' = b { treeFrontier = insert (sampled, objs) treeFrontier,
                     treeNumVisits = treeNumVisits + 1,
                     treeChoiceInfo = updateChoiceInfo chosen treeChoiceInfo}
        return (b', sampled, objs)
      False -> do
        let nextStep = Opt (m (chosenChoice chosen))
        let ix = discreteChosenIndex chosen
        let oldSubtree = expandedChoices treeChoiceInfo IM.! ix
        (newSubtree, sampled, objs) <- mctsAddSample nextStep eval oldSubtree
        let newExpandedChoices = IM.insert ix newSubtree (expandedChoices treeChoiceInfo)
        let b' = b { treeFrontier = insert (sampled, objs) treeFrontier,
                     treeNumVisits = treeNumVisits + 1,
                     treeChoiceInfo = treeChoiceInfo {expandedChoices = newExpandedChoices}}
        return (b', sampled, objs)



-- TODO: this isn't quite right. Nested MCTS records the globally best sequence
-- and always plays the move from that sequence, even if it wasn't found in the
-- current step.

-- | basic nested MCTS, no tree returned.
-- Returns a non-dominated solution if one can be found.
nested :: Opt a
          -> (a -> U.Vector Double)
          -- ^ evaluation function
          -> Int
          -- ^ levels before just random sampling
          -> RVarT IO (Maybe (a, U.Vector Double))
nested (Opt o) eval l = case (view o, l) of
  (Return x, _) -> return $ Just (x, eval x)
  (((UniformChoice xs) :>>= m), 1) -> do
    let subtrees = V.map (Opt . m) xs
    sampled <- (V.map (\x -> (x, eval x))) <$> mapM toRVarT subtrees
    return $ maximumByMay (domOrdering `on` snd) sampled
  (((UniformChoice xs) :>>= m), i) -> do
    subresults <- asum <$> forM xs (\a -> do
                    nestedRes <- nested (Opt $ m a) eval (i-1)
                    return (fmap (\(res,score) -> (a,res,score)) nestedRes))
    let trd (_,_,z) = z
    let fst3 (x,_,_) = x
    let bestMove = fst3 <$> maximumByMay (domOrdering `on` trd) subresults
    case bestMove of
      Nothing -> return Nothing
      Just x -> nested (Opt (m x)) eval (i-1)



