{-# LANGUAGE GADTs #-}

module Multioptimizer.Backend.Internal where

import Control.Monad.Trans.Maybe (MaybeT)
import Data.Random (RVarT)
import qualified Data.Vector.Unboxed as U

import Multioptimizer.Internal

data Backend a where
  Backend :: Monoid m =>
          (Opt a
           -> (a -> IO (U.Vector Double))
           -> m
           -> MaybeT (RVarT IO) (m, a, U.Vector Double))
          -- ^ Sampler
          -> Backend a


