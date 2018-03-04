{-# LANGUAGE GADTs #-}

module Multioptimizer.Backend.Internal where

import Data.Semigroup
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Random (RVarT)
import qualified Data.Vector.Unboxed as U

import Multioptimizer.Internal

data Backend a where
  Backend :: Semigroup s =>
          (Opt a
           -> (a -> IO (U.Vector Double))
           -> s
           -> MaybeT (RVarT IO) (s, a, U.Vector Double))
          -- ^ Sampler
          -> (Opt a -> s)
          -- ^ Initialize sampler state
          -> Backend a


