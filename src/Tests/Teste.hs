
class F a b where
   f :: a -> b
   
   f x = x
   
class O a where
   o :: a

instance F Char Char where
    f 'c' = 'a'
    
instance O Char where
    o = 'c'

h = f o

m = ()

g :: Int -> Int
g x = x

class Num a where
    fromInteger :: Integer -> a

instance Num Integer
