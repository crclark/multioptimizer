module Multioptimizer.Backend.MCTS where

import Data.Word
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.IntMap.Strict as IM
import Multioptimizer.Util.Pareto

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