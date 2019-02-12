--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture 17: Fun with Applicative Functors                                  --
--------------------------------------------------------------------------------

module Lecture17 where

--------------------------------------------------------------------------------
-- Writer type

data Writer w a = MkWriter (a,w)

runWriter :: Writer w a -> (a,w)
runWriter (MkWriter m) = m

tell :: w -> Writer w ()
tell o = MkWriter ((), o)

instance Functor (Writer w) where
    fmap f (MkWriter (x,o)) = MkWriter (f x, o)

ex0 :: Num b => Writer [a] b
ex0 = fmap (+5) $ MkWriter (4,[])

instance Monoid w => Applicative (Writer w) where
    pure x = MkWriter (x, mempty)

    MkWriter (f,o1) <*> MkWriter (x,o2) = MkWriter (f x, o1 `mappend` o2)

ex1 :: Num a => Writer [a] ()
ex1 = (\_ _ -> ()) <$> tell [1,2] <*> tell [3,4]

--------------------------------------------------------------------------------
-- Logging example

-- | Represents log messages comprised of a source (represented as a `String`)
-- and a message (also represented as a `String`).
data LogMessage = LogM String String deriving Show

logM :: String -> String -> Writer [LogMessage] ()
logM source message = tell [LogM source message]

-- data types from our compiler example
data Expr = Val Int | Plus Expr Expr deriving Show
data Instr = PUSH Int | ADD deriving Show
type Program = [Instr]

comp :: Expr -> Writer [LogMessage] Program
comp (Val n)    = logM "comp" "compiling a value" *> pure [PUSH n]
comp (Plus l r) = logM "comp" "compiling a plus" *>
    ((\p p' -> p ++ p' ++ [ADD]) <$> comp l <*> comp r)

ex2 :: Writer [LogMessage] Program
ex2 = comp (Plus (Val 4) (Val 8))

--------------------------------------------------------------------------------
