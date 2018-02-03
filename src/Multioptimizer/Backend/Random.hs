module Multioptimizer.Backend.Random (
  sample
)where

import Multioptimizer.Internal

import Control.Monad.Operational (ProgramViewT(Return,(:>>=)), view)
import Data.Random (sampleState, RVar, uniform)
import Data.Random.Source.PureMT (pureMT)
import qualified Data.Vector as V
import Data.Word

-- | Create a single random sample of the given generator.
sample :: Opt a
          -> Word64
          -- ^ Random seed
          -> a
sample a seed = fst $ sampleState (toRVar a) (pureMT seed)

toRVar :: Opt a-> RVar a
toRVar (Opt o) = case view o of
  Return x -> return x
  ((UniformChoice xs) :>>= m) -> do
    ix <- Data.Random.uniform 0 (V.length xs - 1)
    toRVar $ Opt $ m $ xs V.! ix
