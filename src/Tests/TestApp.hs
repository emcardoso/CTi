
g :: Int -> Int
g x = x

h = g 1

class Num a where
    fromInteger :: Integer -> a

instance Num Int