--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture: Applicative Functors (cont.)                                      --
--------------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor #-}

module Lecture where

--------------------------------------------------------------------------------

import Prelude hiding (Applicative(..))

import System.Random

--------------------------------------------------------------------------------

infixl 4 <*>
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

instance Applicative [] where
    pure x = [x]

    fs <*> xs = [f x | f <- fs, x <- xs]

listEx0 :: (a -> b) -> (a -> b) -> a -> a -> a -> [b]
listEx0 f g x y z = [f,g] <*> [x,y,z]

listEx1 :: (a -> b -> c) -> a -> a -> b -> b -> b -> [c]
listEx1 g x y a b c = g <$> [x,y] <*> [a,b,c]

listEx2 :: Num a => a -> a -> a -> a -> a -> [a]
listEx2 u v a b c = (((\x y z -> x+y+z) <$> [u,v]) <*> [a,b,c]) <*> [1,2]

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
