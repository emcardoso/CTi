module Teste1 where

class Mult a b c where
    (.*.) :: a -> b -> c
    
data Vector = Vector Int Int
data Matrix = Matrix Vector Vector

instance Mult Matrix Matrix Matrix
instance Mult Matrix Vector Matrix
instance Mult Int Int Int

m1, m2, m3 :: Matrix
m1 = Matrix (Vector 1 1) (Vector 2 2)
m2 = Matrix (Vector 1 1) (Vector 2 2)
m3 = Matrix (Vector 1 1) (Vector 2 2)

m :: Matrix
m = (m1 .*. m2) .*. m3

class Num a where
    fromInteger :: Integer -> a

instance Num Integer

instance Num Int
