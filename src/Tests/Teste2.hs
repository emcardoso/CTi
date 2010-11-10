module Teste2 where

class Mult a b c where
    (.*.) :: a -> b -> c
    
instance Mult a b c => Mult a [b] [c] 

f :: (Mult a [b] b) => Bool -> a -> b -> b
f b x y = if b then x .*. [y] else y

