{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLists #-}

module Multioptimizer (
  Opt
  , uniform
  , testOpt
) where

import Multioptimizer.Internal

testOpt :: Opt Int
testOpt = do
  x <- uniform [1, 2, 3, 4]
  y <- uniform [5, 6, 7]
  z <- if x == 1 then return 10 else return 5
  return (x + y + z)
