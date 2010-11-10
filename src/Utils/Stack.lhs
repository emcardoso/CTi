this module implements a simple stack.

> module Utils.Stack (Stack(..), push, pop, null, peek, applyToTop) where

> import Prelude hiding (null)

> newtype Stack a = Stack [a] deriving (Eq, Ord, Show)

> push :: a -> Stack a -> Stack a
> push x (Stack xs) = Stack (x:xs)

> pop :: Stack a -> Maybe (a, Stack a)
> pop (Stack [])    = Nothing
> pop (Stack (x:xs)) = Just (x, Stack xs)

> peek :: Stack a -> Maybe a
> peek (Stack [])    = Nothing
> peek (Stack (x:_)) = Just x

> null :: Stack a -> Bool
> null (Stack []) = True
> null _          = False

> applyToTop :: (a -> a) -> Stack a -> Maybe (Stack a)
> applyToTop f (Stack []) = Nothing
> applyToTop f (Stack (x:xs)) = Just (Stack (f x : xs))
