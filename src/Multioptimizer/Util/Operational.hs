{-# LANGUAGE GADTs, ExistentialQuantification, Rank2Types, ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE ApplicativeDo #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}
-- Search for UndecidableInstances to see why this is needed

{-
NOTE: this is a rough proof of concept of modifying the original Operational
code to support ApplicativeDo-based optimizations. Specifically, I would like
to be able to make n independent choices at once when possible, rather than
pretending each choice is dependent on the last choice.

In the case of tree search, this would allow us to avoid instantiating a lot
of unnecessary branches in the trees. Instead, we would have one branch that
makes a single choice in the cartesian product of all the independent choices it
can see. This gives us the following benefits:

1. lower memory usage because we coalesce n branches into


-}


module Multioptimizer.Util.Operational (
    -- * Synopsis
    -- $synopsis

    -- * Overview
    -- $intro

    -- * Monad
    Program, singleton, ProgramView, view,
    -- $example

    -- * Monad transformer
    ProgramT, ProgramViewT(..), viewT,
    -- $exampleT
    liftProgram,

    ) where

import Control.Monad.Identity
import Control.Monad.Trans

    -- mtl  classes to instantiate.
    -- Those commented out cannot be instantiated. For reasons see below.
-- import Control.Monad.Cont.Class
-- import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
-- import Control.Monad.Writer.Class

{------------------------------------------------------------------------------
    Introduction
------------------------------------------------------------------------------}
{-$synopsis
To write a monad, use the 'Program' type.

To write a monad transformer, use the 'ProgramT' type.

For easier interoperability,
the 'Program' type is actually a type synonym
and defined in terms of 'ProgramT'.
-}

{-$intro

The basic idea for implementing monads with this libary
is to think of monads as /sequences of primitive instructions/.
For instance, imagine that you want to write a web application
with a custom monad that features an instruction

> askUserInput :: CustomMonad UserInput

which sends a form to the remote user and waits for the user
to send back his input

To implement this monad, you decide that this instruction is
a primitive, i.e. should not be implemented in terms of other,
more basic instructions.
Once you have chosen your primitives, collect them in a data type

@
data CustomMonadInstruction a where
    AskUserInput :: CustomMonadInstruction UserInput
@

Then, obtain your custom monad simply by applying the 'Program'
type constructor

> type CustomMonad a = Program CustomMonadInstruction a

The library makes sure that it is an instance of the 'Monad' class
and fulfills all the required laws.

Essentially, the monad you now obtained is just a
fancy list of primitive instructions.
In particular, you can pattern match on the first element of this "list".
This is how you implement an @interpret@ or @run@ function for your monad.
Note that pattern matching is done using the 'view' function

@
runCustomMonad :: CustomMonad a -> IO a
runCustomMonad m = case view m of
    Return a            -> return a -- done, return the result
    AskUserInput :>>= k -> do
        b <- waitForUserInput       -- wait for external user input
        runCustomMonad (k b)        -- proceed with next instruction
@

The point is that you can now proceed in any way you like:
you can wait for the user to return input as shown,
or you store the continuation @k@ and retrieve it when
your web application receives another HTTP request,
or you can keep a log of all user inputs on the client side and replay them,
and so on. Moreover, you can implement different @run@ functions
for one and the same custom monad, which is useful for testing.
Also note that the result type of the @run@ function does not need to
be a monad at all.

In essence, your custom monad allows you to express
your web application as a simple imperative program,
while the underlying implementation can freely map this to
an event-drived model or some other control flow architecture
of your choice.

The possibilities are endless.
More usage examples can be found here:
<https://github.com/HeinrichApfelmus/operational/tree/master/doc/examples#readme>

-}

{------------------------------------------------------------------------------
   Program
------------------------------------------------------------------------------}
{-| The abstract data type @'Program' instr a@ represents programs,
    i.e. sequences of primitive instructions.

    * The /primitive instructions/ are given by the type constructor @instr :: * -> *@.

    * @a@ is the return type of a program.

    @'Program' instr@ is always a monad and
    automatically obeys the monad laws.
-}
type Program instr = ProgramT instr Identity

-- | View type for inspecting the first instruction.
--   It has two constructors 'Return' and @:>>=@.
--   (For technical reasons, they are documented at 'ProgramViewT'.)
type ProgramView instr  = ProgramViewT instr Identity

-- | View function for inspecting the first instruction.
view :: Program instr a -> ProgramView instr a
view = runIdentity . viewT

{-
-- | Utility function that extends
-- a given interpretation of instructions as monadic actions
-- to an interpration of 'Program's as monadic actions.
--
-- This function can be useful if you are mainly interested in
-- mapping a 'Program' to different standard monads, like the state monad.
-- For implementing a truly custom monad,
-- you should write your interpreter directly with 'view' instead.
interpretWithMonad :: forall instr m b.
    Monad m => (forall a. instr a -> m a) -> (Program instr b -> m b)
interpretWithMonad f = eval . view
    where
    eval :: forall a. ProgramView instr a -> m a
    eval (Return a) = return a
    eval (m :>>= k) = f m >>= interpretWithMonad f . k
-}
{- $example

/Example usage/

Stack machine from \"The Operational Monad Tutorial\".

>    data StackInstruction a where
>        Push :: Int -> StackInstruction ()
>        Pop  :: StackInstruction Int
>
>    type StackProgram a = Program StackInstruction a
>    type Stack b        = [b]
>
>    interpret :: StackProgram a -> (Stack Int -> a)
>    interpret = eval . view
>        where
>        eval :: ProgramView StackInstruction a -> (Stack Int -> a)
>        eval (Push a :>>= is) stack     = interpret (is ()) (a:stack)
>        eval (Pop    :>>= is) (a:stack) = interpret (is a ) stack
>        eval (Return a)       stack     = a

In this example, the type signature for the `eval` helper function is optional.

-}

{------------------------------------------------------------------------------
    ProgramT - monad transformer
------------------------------------------------------------------------------}
{-| The abstract data type @'ProgramT' instr m a@ represents programs
    over a base monad @m@,
    i.e. sequences of primitive instructions and actions from the base monad.

    * The /primitive instructions/ are given by the type constructor @instr :: * -> *@.

    * @m@ is the base monad, embedded with 'lift'.

    * @a@ is the return type of a program.

    @'ProgramT' instr m@ is a monad transformer and
    automatically obeys both the monad and the lifting laws.
-}
data ProgramT instr m a where
    Lift   :: m a -> ProgramT instr m a
    Bind   :: ProgramT instr m b -> (b -> ProgramT instr m a)
           -> ProgramT instr m a
    Instr  :: instr a -> ProgramT instr m a
    Ap     :: ProgramT instr m (b -> a)
           -> ProgramT instr m b
           -> ProgramT instr m a

    -- basic instances
instance Monad m => Monad (ProgramT instr m) where
    return = Lift . return
    (>>=)  = Bind

instance MonadTrans (ProgramT instr) where
    lift   = Lift

instance Monad m => Functor (ProgramT instr m) where
    fmap   = liftM

instance Monad m => Applicative (ProgramT instr m) where
    pure   = return
    (<*>)  = Ap

-- | Program made from a single primitive instruction.
singleton :: instr a -> ProgramT instr m a
singleton = Instr

-- | View type for inspecting the first instruction.
-- This is very similar to pattern matching on lists.
--
-- * The case @(Return a)@ means that the program contains no instructions
-- and just returns the result @a@.
--
-- *The case @(someInstruction :>>= k)@ means that the first instruction
-- is @someInstruction@ and the remaining program is given by the function @k@.
data ProgramViewT instr m a where
    Return :: a -> ProgramViewT instr m a
    -- This new case is needed for applicatives. There needs to be a way to
    -- represent a "leaf" instruction whose result doesn't necessarily get bound
    -- but nonetheless needs to be executed in order. In other words, these
    -- are the effects x and y in the expression @f <$> x <*> y@.
    -- TODO: make sure that this doesn't cause any unforeseen problems,
    -- particularly w.r.t. being able to express the same computation in two
    -- ways. Can it interact badly with bind?
    RawInstr :: instr a -> ProgramViewT instr m a
    (:>>=) :: ProgramViewT instr m b
           -> (b -> ProgramT instr m a)
           -> ProgramViewT instr m a
    (:<*>) :: ProgramViewT instr m (b -> a)
           -> ProgramViewT instr m b
           -> ProgramViewT instr m a

-- | View function for inspecting the first instruction.
viewT :: Monad m => ProgramT instr m a -> m (ProgramViewT instr m a)
viewT (Lift m)                = m >>= return . Return
viewT ((Lift m)     `Bind` g) = m >>= viewT . g
viewT ((m `Bind` g) `Bind` h) = viewT (m `Bind` (\x -> g x `Bind` h))
viewT ((Ap f x) `Bind` g)     = do f' <- viewT f
                                   x' <- viewT x
                                   return $ (f' :<*> x') :>>= g
viewT ((Instr i)    `Bind` g) = return ((RawInstr i) :>>= g)
viewT (Instr i)               = return ((RawInstr i) :>>= return)
viewT (Ap f x)                = (:<*>) <$> viewT f <*> viewT x

{-| Lift a plain sequence of instructions to a sequence
    of instructions over a monad 'm'.
    This is the counterpart of the 'lift' function from 'MonadTrans'.

    It can be defined as follows:

@
    liftProgram = eval . view
        where
        eval :: ProgramView instr a -> ProgramT instr m a
        eval (Return a) = return a
        eval (i :>>= k) = singleton i >>= liftProgram . k
@

-}
liftProgram :: Monad m => Program instr a -> ProgramT instr m a
liftProgram (Lift m)     = return (runIdentity m)
liftProgram (m `Bind` k) = liftProgram m `Bind` (liftProgram . k)
liftProgram (Instr i)    = Instr i
liftProgram (Ap f x)     = Ap (liftProgram f) (liftProgram x)


{- $exampleT

/Example usage/

List monad transformer.

>    data PlusI m a where
>        Zero :: PlusI m a
>        Plus :: ListT m a -> ListT m a -> PlusI m a
>
>    type ListT m a = ProgramT (PlusI m) m a
>
>    runList :: Monad m => ListT m a -> m [a]
>    runList = eval <=< viewT
>        where
>        eval :: Monad m => ProgramViewT (PlusI m) m a -> m [a]
>        eval (Return x)        = return [x]
>        eval (Zero     :>>= k) = return []
>        eval (Plus m n :>>= k) =
>            liftM2 (++) (runList (m >>= k)) (runList (n >>= k))

In this example, the type signature for the `eval` helper function is optional.

-}

{------------------------------------------------------------------------------
    mtl instances

  * All of these instances need UndecidableInstances,
    because they do not satisfy the coverage condition.
    Most of the instance in the  mtl  package itself have the same issue.

  * Lifting algebraic operations is easy,
    lifting control operations is more elaborate, but sometimes possible.
    See the design notes in  `doc/design.md`.
------------------------------------------------------------------------------}
instance (MonadState s m) => MonadState s (ProgramT instr m) where
    get = lift get
    put = lift . put

instance (MonadIO m) => MonadIO (ProgramT instr m) where
    liftIO = lift . liftIO

instance (MonadReader r m) => MonadReader r (ProgramT instr m) where
    ask = lift ask

    local r (Lift m)     = Lift (local r m)
    local r (m `Bind` k) = local r m `Bind` (local r . k)
    local _ (Instr i)    = Instr i
    local r (Ap f x)     = (local r f) `Ap` (local r x)

{-
Everything below here is temporary proof of concept
---------------------------------------------------
-}


data FileSystemI a where
    ReadFile :: FilePath -> FileSystemI String
    WriteFile :: FilePath -> String -> FileSystemI ()

type FileSystem a = Program FileSystemI a

readFile' :: FilePath -> FileSystem String
readFile' fp = singleton (ReadFile fp)

writeFile' :: FilePath -> String -> FileSystem ()
writeFile' fp str = singleton (WriteFile fp str)


{-
Without ApplicativeDo, @printFileSystemProgram exampleProg@ yields:

```
ReadFile foo
BIND!
ReadFile bar
BIND!
WriteFile baz 1
BIND!
return something
```

With ApplicativeDo, @printFileSystemProgram exampleProg@ yields:

```
Saw 2 ops.
["ReadFile foo and bind","ReadFile bar and bind"]
ReadFile foo
BIND!
return something
ReadFile bar
BIND!
return something
WriteFile baz 1
BIND!
return something
```

NOTE that we were able to 1. count the number of visible ops and 2. make a list
of the visible ops *without* needing to execute them immediately. We could do
any kind of fold we wanted over those ops, and then passed them into a different
interpreter based on what we saw.

-}

exampleProg :: FileSystem Int
exampleProg = do
    x <- readFile' "foo"
    y <- readFile' "bar"
    writeFile' "baz" x
    return (read y)

-- | Counts as many ops as can be found without returning any.
countVisibleOps :: ProgramViewT FileSystemI Identity a -> Int
countVisibleOps (Return _) = 0
countVisibleOps (RawInstr (ReadFile _)) = 1
countVisibleOps (RawInstr (WriteFile _ _)) = 1
countVisibleOps (x :>>= _) = countVisibleOps x
countVisibleOps (f :<*> x) = countVisibleOps f + countVisibleOps x

-- TODO: the code below is quite weird. Simplify.

gatherVisibleAp :: ProgramViewT FileSystemI Identity a -> [String]
gatherVisibleAp (Return _) = ["return x"]
gatherVisibleAp (RawInstr (ReadFile fp)) = ["ReadFile " ++ fp]
gatherVisibleAp (RawInstr (WriteFile fp str)) = ["WriteFile " ++ fp ++ " " ++ str]
gatherVisibleAp ((RawInstr (ReadFile fp)) :>>= _)
  = ["ReadFile " ++ fp ++ " and bind"]
gatherVisibleAp ((RawInstr (WriteFile fp str)) :>>= _)
  = ["WriteFile " ++ fp ++ " " ++ str ++ " and bind"]
gatherVisibleAp (f :<*> x) = gatherVisibleAp f ++ gatherVisibleAp x
gatherVisibleAp (x :>>= _) = gatherVisibleAp x

printFileSystemProgram :: FileSystem a -> IO a
printFileSystemProgram fs = go (view fs)
    where go :: ProgramViewT FileSystemI Identity a -> IO a
          go fs' = case fs' of
                    (Return x) -> do putStrLn $ "return something"
                                     return x
                    (RawInstr (ReadFile fp)) -> do putStrLn $ "ReadFile " ++ fp
                                                   return "1"
                    (RawInstr (WriteFile fp str)) -> putStrLn $ "WriteFile " ++ fp ++ " " ++ str
                    ((RawInstr (ReadFile fp)) :>>= m)
                      -> do putStrLn $ "ReadFile " ++ fp
                            putStrLn "BIND!"
                            -- ReadFile ops arbitrarily produce "1"
                            printFileSystemProgram (m "1")
                    ((RawInstr (WriteFile fp str)) :>>= m)
                      -> do putStrLn $ "WriteFile " ++ fp ++ " " ++ str
                            putStrLn "BIND!"
                            printFileSystemProgram (m ())
                    a@(f :<*> x) -> do putStrLn $ "Saw " ++ (show (countVisibleOps a)) ++ " ops."
                                       putStrLn (show (gatherVisibleAp a))
                                       (go f) <*> (go x)
                    (x :>>= m) -> do x' <- go x
                                     printFileSystemProgram (m x')
