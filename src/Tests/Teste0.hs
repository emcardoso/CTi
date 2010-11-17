
data Bool = True | False  

id :: a -> a
id x = x

g :: Char -> Char
g x = x
 
t :: Bool
t = True
 
f :: (Char -> Char ^ Bool -> Bool) -> (Char, Bool)
f x = (x 'a', x t)

app1 :: (Char, Bool)
app1 = f id

app2 = f h 

class C a where
  h :: a -> a

instance C Char
instance C Int