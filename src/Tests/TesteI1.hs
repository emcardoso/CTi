data Bool = True | False  

id :: a -> a
id x = x 

m :: ([Char] -> a ^ Bool -> Bool) -> (a,Bool)
m x = (x ['a'], x True)

g = m id 