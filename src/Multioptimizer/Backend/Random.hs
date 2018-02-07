module Multioptimizer.Backend.Random (
  sample
, toRVar
, toRVarT
) where

import Multioptimizer.Internal

import Control.Monad.Operational (ProgramViewT(Return,(:>>=)), view)
import Data.Random (sampleState, RVar, RVarT, uniformT)
import Data.Random.Source.PureMT (pureMT)
import qualified Data.Vector as V
import Data.Word

-- TODO: the functions here aren't total. Consider if UniformChoice is
-- passed an empty vector.

-- | Create a single random sample of the given generator.
sample :: Opt a
          -> Word64
          -- ^ Random seed
          -> a
sample a seed = fst $ sampleState (toRVar a) (pureMT seed)

toRVar :: Opt a -> RVar a
toRVar = toRVarT

toRVarT :: Monad m => Opt a-> RVarT m a
toRVarT (Opt o) = case view o of
  Return x -> return x
  ((UniformChoice xs) :>>= m) -> do
    ix <- Data.Random.uniformT 0 (V.length xs - 1)
    toRVarT $ Opt $ m $ xs V.! ix
