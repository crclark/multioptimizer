module Multioptimizer.Backend.MCTS where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

data Node = Node {}
  deriving Show

data Tree = Leaf (U.Vector Double) | Branch Node (V.Vector Node)
  deriving Show