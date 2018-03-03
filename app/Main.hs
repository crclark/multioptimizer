{-# LANGUAGE OverloadedLists #-}

module Main where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef)
import Data.Random (RVarT, runRVarTWith, uniformT)
import Data.Random.Source.PureMT (newPureMT)
import GHC.Exts(IsList(..))

import Multioptimizer
import Multioptimizer.Backend.MCTS
import Multioptimizer.Util.Pareto

loop :: RVarT IO ()
loop = forM_ ([1..100] :: [Int]) $ \_ -> do
  i <- uniformT 0 5
  liftIO $ print (i :: Int)

main2 :: IO ()
main2 = do
  mt <- newPureMT
  refmt <- newIORef mt
  runRVarTWith liftIO loop refmt

main :: IO ()
main = do
  let gen = uniform [1.0,2.0,3.0]
  result <- mcts defaultOpts{maxSolutions = 1,
                             timeLimitMillis = 1} gen (\x -> return [x])
  let resNums = map fst (toList result)
  print resNums
