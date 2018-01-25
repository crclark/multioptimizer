{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Multioptimizer.Internal where

import Control.Monad.Operational (Program, singleton)
import Data.Vector (Vector)

data OptInstr a where
  UniformChoice :: Vector a -> OptInstr a
  --Maximize :: Double -> OptInstr ()

deriving instance Show a => Show (OptInstr a)

newtype Opt a = Opt {getProgram :: Program OptInstr a}

deriving instance Functor Opt
deriving instance Applicative Opt
deriving instance Monad Opt

uniform :: Vector a -> Opt a
uniform = Opt . singleton . UniformChoice