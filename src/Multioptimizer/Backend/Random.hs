{-# LANGUAGE TypeApplications #-}

module Multioptimizer.Backend.Random (
  sample
, runOpt
) where

import Multioptimizer.Internal

import Data.Functor.Identity (Identity)
import Control.Monad.Operational (ProgramViewT(Return,(:>>=)), view)
import Data.Random (sampleState, RVarT, uniformT)
import Data.Random.Source.PureMT (pureMT)
import qualified Data.Vector as V
import Data.Word

-- | Create a single random sample of the given generator.
-- We return 'Maybe a' because a generator can fail to produce a value.
-- Consider @uniformChoice []@.
sample :: Opt a
          -> Word64
          -- ^ Random seed
          -> Maybe a
sample a seed = fst $ sampleState (runOpt @ Identity a) (pureMT seed)

runOpt :: Monad m => Opt a-> RVarT m (Maybe a)
runOpt (Opt o) = case view o of
  Return x -> return (Just x)
  ((UniformChoice xs) :>>= m) -> do
    if V.null xs
      then return Nothing
      else do
        ix <- Data.Random.uniformT 0 (V.length xs - 1)
        runOpt $ Opt $ m $ xs V.! ix
