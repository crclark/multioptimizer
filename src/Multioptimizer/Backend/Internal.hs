{-# LANGUAGE GADTs #-}

module Multioptimizer.Backend.Internal where

import Control.Monad.Trans.Maybe (MaybeT)
import Data.Random (RVar)
import qualified Data.Vector.Unboxed as U

import Multioptimizer.Internal

-- TODO: MaybeT RVar newtype

data Backend a where
  Backend :: Monoid m =>
          (Opt a
           -> m
           -- ^ Search state
           -> MaybeT RVar ([Breadcrumb], a))
          -- ^ Sampler
          -> ([Breadcrumb] -> U.Vector Double -> m -> m)
          -- ^ Update state based on search result
          -> Backend a


