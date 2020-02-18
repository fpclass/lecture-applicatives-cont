--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture: Fun with Applicative Functors                                     --
--------------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor #-}

module Lecture where

import System.Random

--------------------------------------------------------------------------------
-- Writer type

-- the expression language and the instruction set for the stack-based machine
-- from one of the previous lectures
data Expr = Val Int | Plus Expr Expr deriving Show 
data Instr = PUSH Int | ADD deriving Show 
type Program = [Instr]

data Writer w a = MkWriter (a,w) deriving Show

instance Functor (Writer w) where
    fmap f (MkWriter (x,o)) = MkWriter (f x, o)

writeLog :: String -> Writer [String] ()
writeLog msg = MkWriter ((), [msg])

instance Monoid w => Applicative (Writer w) where
    pure x = MkWriter (x, mempty)

    MkWriter (f,o1) <*> MkWriter (x,o2) = MkWriter (f x, o1 <> o2)

comp :: Expr -> Writer [String] Program
comp (Val n)    = writeLog "compiling a value" *> pure [PUSH n]
comp (Plus l r) = writeLog "compiling a plus" *>
    ((\p p' -> p ++ p' ++ [ADD]) <$> comp l <*> comp r)

compEx :: Writer [String] Program
compEx = comp (Plus (Val 4) (Val 8))

--------------------------------------------------------------------------------

data State s a = St (s -> (a,s))
    deriving Functor -- lab exercise!

runState :: State s a -> s -> (a,s)
runState (St m) = m

instance Applicative (State s) where
   pure x = St (\s -> (x,s))

   St mf <*> St mx = St (\s -> let (f,s') = mf s
                                   (x,s'') = mx s'
                               in (f x,s''))

--------------------------------------------------------------------------------
-- Random number generation

randomNumber :: State StdGen Int
randomNumber = St random

twoRandomNumbers :: State StdGen (Int, Int)
twoRandomNumbers = mkPair <$> randomNumber <*> randomNumber
   where mkPair a b = (a,b)

--------------------------------------------------------------------------------
