
g :: Int -> Int
g 0 = 1
g 1 = 5
g x = x

h = g 1

class Num a where
    fromInteger :: Integer -> a

instance Num Int