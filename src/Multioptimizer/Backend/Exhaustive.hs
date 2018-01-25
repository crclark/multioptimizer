{-# LANGUAGE GADTs #-}
module Multioptimizer.Backend.Exhaustive (
  search
)
 where

import Multioptimizer.Internal
import Multioptimizer.Util.Pareto (emptyFrontier, insert, getFrontier, shrinkToSize)
import Control.Monad.Operational (ProgramViewT(Return,(:>>=)), view)
import Control.Monad.Trans (lift)
import Data.List (maximumBy)
import Data.Ord (comparing)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import GHC.Exts(IsList(..))
import ListT (ListT, fromFoldable, fold)

search :: Monad m => Opt a
                  -- ^ Generator.
                  -> Word
                  -- ^ Maximum number of solutions on the Pareto front
                  -- to return.
                  -> (a -> m (U.Vector Double))
                  -- ^ Evaluation function. Must always return vectors of the
                  -- same length for valid results!
                  -> m (V.Vector (a, U.Vector Double))
                  -- ^ Vector of Pareto front of items, with their
                  -- evaluations.
search a maxSize f = do
  front <- fold insertOne emptyFrontier (enumerateAll a f)
  return (getFrontier front)
  where insertOne front item =
         let front' = insert item front
             in if V.length (getFrontier front') > (fromIntegral maxSize)
                  then return (shrinkToSize maxSize front')
                  else return front'

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