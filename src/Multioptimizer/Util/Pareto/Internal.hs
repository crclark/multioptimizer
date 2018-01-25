{-# LANGUAGE RecordWildCards #-}

module Multioptimizer.Util.Pareto.Internal where

import Data.Ord (comparing)
import Data.Semigroup
import qualified Data.Vector as V
import Data.Vector.Algorithms.Merge as Merge
import qualified Data.Vector.Unboxed as U
import GHC.Float (floatRange)

-- @dominates x y@ is True iff x Pareto dominates y.
dominates :: U.Vector Double -> U.Vector Double -> Bool
dominates x y =
  U.any id (U.zipWith (>) x y) && U.all id (U.zipWith (>=) x y)

-- | Maintains a set of mutually non-dominated items of a
-- configurable size. Attempts to keep a wide frontier, where items are not
-- crowded near each other in objective space.
-- The algorithm for maintaining the frontier is essentially PAES, augmented
-- with the decrowding algorithm from NSGA-II.
-- See Knowles and Corne 2000, and Deb 2002.
data Frontier a = Frontier
  { frontier :: !(V.Vector (a, U.Vector Double))
    -- ^ objects stored with their objective vectors
  , maxItems :: !Word
    -- ^ max items to keep in frontier before shrinking it
  , numObjs :: !Word
    -- ^ Number of objectives in each objective vector
  }

-- Retrieves the non-dominated items currently in the Frontier.
getFrontier :: Frontier a -> V.Vector (a, U.Vector Double)
getFrontier = frontier . shrinkToSize

emptyFrontier :: Word
                 -- ^ Maximum number of non-dominated solutions to maintain.
                 -> Word
                 -- ^ Number of objectives returned by objective function.
                 -> Frontier a
emptyFrontier maxItems numObjs = Frontier V.empty maxItems numObjs

-- | Inserts an item into the Paerto Frontier. Returns a tuple of the new
-- Frontier and True if the inserted item was non-dominated (kept) or False if
-- the inserted item was already worse than what was already present (and thus
-- discarded).
insertQuery :: (a, U.Vector Double) -> Frontier a -> (Frontier a, Bool)
insertQuery x f@Frontier{..} =
  case nonDominated of
    Nothing -> (f, False) -- new item was dominated by something already in frontier
    Just xs -> let newFrontier = V.fromList xs
                   in if V.length newFrontier > 2*(fromIntegral maxItems)
                         then (shrinkToSize f{frontier = newFrontier}, True)
                         else (f{frontier = newFrontier}, True)
    where nonDominated = foldr go (Just [x]) (V.toList frontier)
          go _ Nothing = Nothing
          go y l@(Just ys) | dominates (snd y) (snd x) = Nothing
                           | dominates (snd x) (snd y) = l
                           | otherwise = Just (y:ys)

-- | Inserts an item and its objective vector into the Pareto Frontier.
-- The item will be discarded if it is dominated by an item already in the
-- Frontier.
insert :: (a, U.Vector Double)
          -- ^ item, objective vector
          -> Frontier a
          -> Frontier a
insert x f = fst (insertQuery x f)

-- | Scale all objectives to be between 0 and 1
scaleObjs :: V.Vector (U.Vector Double) -> V.Vector (U.Vector Double)
scaleObjs vs =
  if V.length vs < 2
     then V.map (U.map (const 1.0)) vs
     else let maxes = V.foldr1 (U.zipWith max) vs
              in V.map (\v -> U.zipWith (/) v maxes) vs

-- | Crowding distances computation from the NSGA-II paper, Deb 2002.
-- @O(m*n*log(n))@ where @m@ is number of objectives and @n@ is number of items
-- in the frontier.
crowdingDists :: Frontier a -> V.Vector Double
crowdingDists Frontier{..} =
  foldr1 (V.zipWith (+))
         (map mthCrowdingDists [0..(fromIntegral numObjs)-1])
   where mthCrowdingDists :: Int -> V.Vector Double
         mthCrowdingDists m =
          let n = V.length frontier
              sorted = V.modify (Merge.sortBy (comparing (U.! m)))
                                (V.map snd frontier)
              mMax = (sorted V.! (n-1)) U.! m
              mMin = (sorted V.! 0) U.! m
              denom = mMax - mMin
              in V.generate n
                            (\i -> if i == 0 || i == (n-1)
                                      then bigDouble
                                      else let rN = (sorted V.! (i+1)) U.! m
                                               lN = (sorted V.! (i-1)) U.! m
                                               in (rN - lN)/denom)

-- | Shrinks frontier to its maximum size, preferring the least-crowded members.
shrinkToSize :: Frontier a -> Frontier a
shrinkToSize f@Frontier{..} =
  let sortedByCrowding = V.modify (Merge.sortBy (comparing $ negate . snd))
                         (V.zip frontier (crowdingDists f))
      in f{frontier = V.take (fromIntegral maxItems)
                             (V.map fst sortedByCrowding)}

-- | https://stackoverflow.com/a/1780582
-- TODO: this becomes Inf after being put in the crowdingDists vector. Bug?
bigDouble :: Double
bigDouble = encodeFloat m n where
    b = floatRadix bigDouble
    e = floatDigits bigDouble
    (_, e') = floatRange bigDouble
    m = b ^ e - 1
    n = e' - e