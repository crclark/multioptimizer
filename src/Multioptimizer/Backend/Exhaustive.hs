{-# LANGUAGE GADTs #-}
module Multioptimizer.Backend.Exhaustive (
  search
)
 where

import Multioptimizer.Internal
import Multioptimizer.Util.Pareto (emptyFrontier, insert, getFrontier)
import Control.Monad.Operational (ProgramViewT(Return,(:>>=)), view)
import Control.Monad.Trans (lift)
import Data.List (maximumBy)
import Data.Ord (comparing)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import GHC.Exts(IsList(..))
import ListT (ListT,fromFoldable, fold)

search :: Monad m => Opt a
                  -- ^ Generator.
                  -> Word
                  -- ^ Maximum number of solutions on the Pareto front
                  -- to return.
                  -> (a -> m (U.Vector Double))
                  -- ^ Evaluation function. Must always return vectors of the
                  -- same length for valid results!
                  -> Word
                  -- ^ Number of objectives in vector returned by evaluation fn.
                  -> m (V.Vector (a, U.Vector Double))
                  -- ^ Vector of Pareto front of items, with their
                  -- evaluations.
search a m f n = do
  let acc front item = return (insert item front)
  front <- fold acc (emptyFrontier m n) (enumerateAll a f)
  return (getFrontier front)

-- | Enumerate all possibilities, paired with their objectives.
enumerateAll :: Monad m =>
                Opt a
                -> (a -> m (U.Vector Double))
                -> ListT m (a, U.Vector Double)
enumerateAll (Opt o) eval =
  case view o of
    Return a -> do objs <- lift (eval a)
                   return (a, objs)
    ((UniformChoice xs) :>>= m) -> do
      x <- fromFoldable xs
      enumerateAll (Opt (m x)) eval