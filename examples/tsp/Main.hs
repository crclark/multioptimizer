{-# LANGUAGE OverloadedLists #-}

{-
The traveling salesman problem is not well-suited for MCTS, but let's try it
anyway.

-}

module Main where

import Multioptimizer (Opt, uniform)
import Multioptimizer.Backend.MCTS (mcts, MCTSOpts(..), defaultOpts)
import Multioptimizer.Executor.Local (Options(..), defaultOptions, runSearch, SearchResult(..))

import Control.Monad.State.Strict (StateT, evalStateT, get, put, lift)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as U
import GHC.Exts(IsList(..))
import Text.Read (readMaybe)
import System.Environment (getArgs)

loadTSP :: FilePath -> IO [(Double, Double)]
loadTSP fp = mapMaybe parseLine . drop 6 . lines <$> readFile fp

  where parseLine :: String -> Maybe (Double, Double)
        parseLine s = case words s of
          [_,x,y] -> (,) <$> readMaybe x <*> readMaybe y
          _ -> Nothing

type TSP a = StateT (Set (Double, Double)) Opt a

tsp :: TSP [(Double, Double)]
tsp = do
  remainingCoords <- get
  if S.size remainingCoords == 0
     then return []
     else do
       let coordVec = fromList $ toList remainingCoords
       next <- lift (uniform coordVec)
       put (S.delete next remainingCoords)
       rest <- tsp
       return $ next : rest

dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1, y1) (x2, y2) = sqrt $ (x1 - x2)**2 + (y1 - y2)**2

-- NOTE: optimal distance for TSP a280 is 2579.
totalDist :: [(Double, Double)] -> Double
totalDist [] = 0.0
totalDist [_] = 0.0
totalDist (p1:p2:xs) = dist p1 p2 + totalDist (p2:xs)

scoreTSP :: [(Double, Double)] -> IO (U.Vector Double)
scoreTSP xs = return [negate (totalDist xs)]

tspOpts :: Word -> Options
tspOpts lim = defaultOptions
  { timeLimitMillis = lim*1000
  , randomSeed = Just 12
  , verbose = True
  , numThreads = 15}

main :: IO ()
main = do
  args <- getArgs
  if length args /= 2
     then print "need two args: time limit in secs and filepath of .tsp file."
     else do
      let lim = read (head args)
      coords <- loadTSP (args !! 1)
      let tspOpt = evalStateT tsp (fromList coords)
      result <- runSearch (tspOpts lim)
                          tspOpt
                          scoreTSP
                          (mcts defaultOpts)
      print $ snd $ head $ toList $ resultFront result
