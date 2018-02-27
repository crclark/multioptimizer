{-# LANGUAGE TypeApplications #-}

module Multioptimizer.Backend.Random (
  sample
, runOpt
) where

import Multioptimizer.Internal

import Data.Functor.Identity (Identity)
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
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
sample a seed =
  let randAction = runMaybeT $ runOpt @ Identity a
      in fst $ sampleState randAction (pureMT seed)

runOpt :: Monad m => Opt a -> MaybeT (RVarT m) a
runOpt (Opt o) = case view o of
  Return x -> return x
  ((UniformChoice xs) :>>= m) -> do
    if V.null xs
      then mzero
      else do
        ix <- lift $ Data.Random.uniformT 0 (V.length xs - 1)
        runOpt $ Opt $ m $ xs V.! ix
