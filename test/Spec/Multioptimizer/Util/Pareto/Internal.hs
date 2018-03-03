{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}

module Spec.Multioptimizer.Util.Pareto.Internal where

import Multioptimizer.Util.Pareto.Internal
import Data.List (sort)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import GHC.Exts(IsList(..))
import GHC.Float


import Test.Tasty
import Test.Tasty.HUnit as HU (testCase, assertEqual, assertBool, (@?=))

units :: TestTree
units = testGroup "Pareto internals unit tests"
                  [ insertEmpty
                  , dominatesUnit
                  , crowdingDistsTwoItems
                  , crowdingDistsManyItems
                  , shrinkToSizeUnit
                  , shrinkRemovesCrowded
                  , bigDoubleIsLarge
                  , insertionOrder
                  , hypervolume2d
                  , hypervolume3d
                  ]

standardEmpty :: Frontier ()
standardEmpty = emptyFrontier

obj :: U.Vector Double -> ((), U.Vector Double)
obj = ((),)

insertEmpty :: TestTree
insertEmpty = testCase "inserting into empty Frontier" $ do
  let vec = obj [1,2]
  let f = insert vec standardEmpty
  assertEqual "get out single inserted element" (getFrontier f) [vec]

dominatesUnit :: TestTree
dominatesUnit = testCase "dominance" $ do
  dominates [1,2] [2,1] @?= False
  dominates [2,2] [2,1] @?= True
  dominates [2,1] [2,2] @?= False
  dominates [2,1] [2,1] @?= False
  dominates [] [] @?= False

crowdingDistsTwoItems :: TestTree
crowdingDistsTwoItems = testCase "Two item frontier -- high crowding dists" $ do
  let f = insert (obj [1,2]) $ insert (obj [2,1]) standardEmpty
  let inf = 1.0/0.0
  crowdingDists f @?= [inf,inf]

crowdingDistsManyItems :: TestTree
crowdingDistsManyItems = testCase "Crowding distance of many items" $ do
  let toInsert = V.map obj
                 [ [1.0, 2.0]
                 , [2.0, 1.0]
                 , [1.95, 1.05]
                 , [1.9, 1.1]
                 , [1.8, 1.2]
                 , [1.7, 1.3]
                 , [1.6, 1.4]
                 , [1.5, 1.5]
                 , [0,0]]
  let f = V.foldr insert standardEmpty toInsert
  let f' = V.foldr insert standardEmpty (V.reverse toInsert)
  let fDists = crowdingDists f
  let f'Dists = crowdingDists f'
  fDists @?= f'Dists

shrinkToSizeUnit :: TestTree
shrinkToSizeUnit = testCase "Shrinking limits size" $ do
  let f :: Frontier ()
      f = V.foldr insert standardEmpty . V.map obj $
                                       [ [1.0, 2.0]
                                       , [2.0, 1.0]
                                       , [1.95, 1.05]
                                       , [1.9, 1.1]
                                       , [1.8, 1.2]
                                       , [1.7, 1.3]]
  length (getFrontier (shrinkToSize 5 f)) @?= 5

shrinkRemovesCrowded :: TestTree
shrinkRemovesCrowded = testCase "Shrinking removes most crowded items" $ do
  let f :: Frontier ()
      f = V.foldr insert standardEmpty . V.map obj $
                                       [ [1.0, 2.0]
                                       , [2.0, 1.0]
                                       , [1.95, 1.05]
                                       , [1.9, 1.1]
                                       , [1.89, 1.11]
                                       , [1.8, 1.2]
                                       , [1.7, 1.3]]
  let front = toList $ getFrontier $ shrinkToSize 5 f
  -- one of the two crowded items has been removed
  (elem (obj [1.89, 1.11]) front && elem (obj [1.9, 1.1]) front) @?= False

bigDoubleIsLarge :: TestTree
bigDoubleIsLarge = testCase "bigDouble is large" $ do
  assertBool "big double * 1.1 is infinite"
             (isInfinite (bigDouble * 1.1))
  assertBool "big double is not infinite"
             (not (isInfinite bigDouble))

insertionOrder :: TestTree
insertionOrder = testCase "insertion order doesn't affect Frontier output" $ do
  let toInsert = V.map obj
                 [ [1.0, 2.0]
                 , [2.0, 1.0]
                 , [1.95, 1.05]
                 , [1.9, 1.1]
                 , [1.8, 1.2]
                 , [1.7, 1.3]
                 , [1.6, 1.4]
                 , [1.5, 1.5]
                 , [0,0]]
  let f = V.foldr insert standardEmpty toInsert
  let f' = V.foldr insert standardEmpty (V.reverse toInsert)
  let fOut = sort $ map (toList . snd) $ toList $ frontier f
  let f'Out = sort $ map (toList . snd) $ toList $ frontier f'
  fOut @?= f'Out

hypervolume2d :: TestTree
hypervolume2d = testCase "some hypervolume test cases" $ do
  let vecs = [[1,2],[2,1]]
  wfg [0,0] vecs @?= 3
  wfg [2,2] vecs @?= -1
  wfg [-1,-1] vecs @?= 8

hypervolume3d :: TestTree
hypervolume3d = testCase "3d hypervolume test cases" $ do
  wfg [0,0,0] [[1,2,3]] @?= 6
  wfg [0,0,1] [[1,2,3]] @?= 4
  wfg [0,0,0] [[1,1,2], [2,1,1]] @?= 3
