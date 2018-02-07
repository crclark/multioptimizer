module Multioptimizer.Backend.MCTS where

import Control.Monad.Operational (ProgramViewT(Return,(:>>=)), view)
import Data.Word
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

data ChoiceInfo a = DiscreteChoice {
  discreteChoiceMax :: Word,
  -- ^ choices are in the range 0,discreteChoiceMax, inclusive
  expandedChoices :: IM.IntMap (Tree a)
  -- ^ branches to choices that have already been expanded
  } deriving Show

data Tree a = Branch {
  treeFrontier :: Frontier a,
  treeNumVisits :: Word,
  treeChoiceInfo :: ChoiceInfo a
}
  deriving Show

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



