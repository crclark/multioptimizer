{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Multioptimizer.Internal where

import Control.Monad.Operational (Program, singleton)
import Data.Vector (Vector)

-- TODO: add an OrderedUniformChoice instruction for the case where the input
-- choices have a natural ordering. This allows us to repeatedly bisect them,
-- and thus share reward information between nearby values.
-- See: DeepArchitect: Automatically Designing and Training Deep Architectures
-- by Negrinho and Gordon, 2017.
-- That paper finds that MCTS doesn't outperform random search without that
-- optimization.
-- I believe we could add this without adding a new OptInstr by creating a
-- helper combinator that translates into a tree of UniformChoices.
-- On the other hand, if we did add another OptInstr, the interpreter could
-- choose to do something sophisticated with it, or translate it into a bisect
-- operation then.

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
