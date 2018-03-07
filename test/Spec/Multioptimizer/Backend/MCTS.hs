{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Spec.Multioptimizer.Backend.MCTS where

import Multioptimizer
import Multioptimizer.Backend.MCTS
import Multioptimizer.Backend.MCTS.Internal
import Multioptimizer.Executor.Local
import Multioptimizer.Util.Pareto

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import GHC.Exts(IsList(..))

import Test.Tasty
import Test.Tasty.HUnit as HU (testCase, assertEqual, assertBool, (@?=))

units :: TestTree
units = testGroup
  "MCTS tests"
  [ maximizeEmpty
  , maximizeTrivial
  , uctUnvisited
  , maximizeRecursive
  , maximizeRecursiveNested
  , maximizeSum
  ]

run opts o f = resultFront <$> runSearch opts o f (mcts defaultOpts)

maximizeEmpty :: TestTree
maximizeEmpty = testCase "maximize empty problem" $ do
  let gen = uniform []
  result <- run defaultOptions gen (\x -> return [x])
  let res = toList result
  assertEqual "Result is empty" [] res

maximizeTrivial :: TestTree
maximizeTrivial = testCase "maximize trivial objective" $ do
  let gen = uniform [1.0, 2.0, 3.0]
  result <- run defaultOptions { maxSolutions = 1, timeLimitMillis = 10 }
                 gen
                 (\x -> return [x])
  let resNums = map fst (toList result)
  assertEqual "Pareto front has one occupant" 1   (length resNums)
  assertEqual "discovered solution is three"  3.0 (head resNums)

uctUnvisited :: TestTree
uctUnvisited = testCase "uct prefers unvisited branches" $ do
  let visFunc i = if i == 0 then 0.0 else 1.0
  let res = uct 2 0.2 visFunc (fromList [(1, 100.0), (2, 100.0)]) 2
  assertEqual "Ceteris paribus, unvisited branch preferred" 0 res

data TTree = TBranch TTree TTree | TLeaf
  deriving (Show, Eq)

scoreTree :: TTree -> IO (U.Vector Double)
scoreTree t = return [go 1.0 t]
 where
  go :: Double -> TTree -> Double
  go s TLeaf         = s
  go s (TBranch l r) = go (s * 2.0) l + go s r

genTreeOfDepth :: Int -> Opt TTree
genTreeOfDepth 0 = return TLeaf
genTreeOfDepth n = do
  goLeft  <- uniform [True, False]
  subtree <- genTreeOfDepth (n - 1)
  if goLeft
    then return $ TBranch subtree TLeaf
    else return $ TBranch TLeaf subtree

maximizeRecursive :: TestTree
maximizeRecursive = testCase "mcts finds optimal path through small tree" $ do
  result <- run defaultOptions { maxSolutions = 1 } (genTreeOfDepth 3) scoreTree
  let resTree = head $ map fst $ toList result
  assertEqual "found leftward tree"
              (TBranch (TBranch (TBranch TLeaf TLeaf) TLeaf) TLeaf)
              resTree

maximizeRecursiveNested :: TestTree
maximizeRecursiveNested =
  testCase "nested mcts finds optimal path through small tree" $ do
    (Just (x, _)) <- nestedMCTS defaultOpts (genTreeOfDepth 3) scoreTree 3 Nothing
    assertEqual "found leftward tree"
                (TBranch (TBranch (TBranch TLeaf TLeaf) TLeaf) TLeaf)
                x

n5s :: Int -> Opt [Int]
n5s 1 = uniform [1 .. 5] >>= return . return
n5s n = do
  x  <- uniform [1 .. 5]
  xs <- n5s (n - 1)
  return (x : xs)

maximizeSum :: TestTree
maximizeSum = testCase "mcts maximizes sum of list" $ do
  result <- run defaultOptions { maxSolutions = 1, timeLimitMillis = 3000 }
                 (n5s 7)
                 (return . U.singleton . sum . map fromIntegral)
  let resList = head $ map fst $ toList result
  assertEqual "all 5s" (replicate 7 5) resList
