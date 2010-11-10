module Prelude (
    module PreludeList, module PreludeText, module PreludeIO,
    Bool (False, True), 
    Maybe (Nothing, Just),
    Either (Left, Right),
    Ordering (LT, EQ, GT),
    Char, String, Int, Integer, Float, Double, Rational, IO,
    Eq ((==), (/=)),
    Ord (compare, (<), (<=), (>), (>=), max, min),
    Enum (succ, pred, toEnum, fromEnum, enumFrom, enumFromThen, enumFromTo, enumFromThenTo),
    Bounded (minBound, maxBound),
    Num ((+), (-), (*), negate, abs, signum, fromInteger),
    Real (toRational),
    Integral (quot, rem, div, mod, quotRem, divMod, toInteger),
    Fractional ((/), recip, fromRational),
    Floating (pi, exp, log, sqrt, (**), logBase, sin, cos, tan, 
              asin, acos, atan, sinh, cosh, tanh, asinh, aconh, atan),
    RealFrac (properFraction, truncate, round, ceiling, floor),
    RealFloat (floatRadix, floatDigits, floatRange, decodeFloat, 
               encodeFloat, exponent, significand, scaleFloat, isNaN,
               isInfinite, isDenormalized, isIEEE, isNegativeZero, atan2),
    Monad (return, fail, (>>=), (>>))
    Functor (fmap),
    mapM, mapM_, sequence, sequence_, (=<<),
    maybe, either,
    (&&), (||), not, otherwise,
    subtract, even, odd, gcd, lcm, (^), (^^),
    fromIntegral, realToFrac,
    fst, snd, curry, uncurry, id, const, (.), flip, ($), until,
    asTypeOf, error, undefined, seq, ($!)) where
    
import PreludeBuiltIn                      -- contains primitive values
import UnicodePrims (primUnicodeMaxChar)   -- primitives for unicode
import PreludeList
import PreludeText
import PreludeIO
import Ratio (Rational)


-- Prelude infix declarations

infixr 9 .
infixr 8 ^, ^^, **
infixl 7 *, / , `quot`, `rem`, `div`, `mod`
infixl 6 +, -

infix 4 == , /= , < , <= , >= , >
infixr 3 &&
infixr 2 ||
infixl 1 >> , >>=
infixr 1 =<<
infixr 0 $ , $! , `seq`

-- Standard types, classes, instances and related functions

-- Equality and Ordering

class Eq a where
    (==), (/=) :: a -> a -> Bool
    
    x == y = not (x /= y)
    x /= y = not (x == y)
    
class Eq a => Ord a where
    compare :: a -> a -> Ordering
    (<), (<=), (>=), (>) :: a -> a -> Bool
    max, min :: a -> a -> a
    
    -- minimal defs. (<=) or compare
    
    compare x y 
        | x == y    = EQ
        | x <= y    = LT
        | otherwise = GT
        
    x <= y = compare x y /= GT
    x <  y = compare x y == LT
    x >  y = compare x y == GT
    x >= y = compare x y /= LT
    
    max x y 
        | x <= y    = y
        | otherwise = x
        
    min x y 
        | x <= y    = x
        | otherwise = y


-- Enumeration and Bounded Classes

class Enum a where
    succ, pred     :: a -> a
    toEnum         :: Int -> a
    fromEnum       :: a -> Int
    enumFrom       :: a -> [a]
    enumFromThen   :: a -> a -> [a]
    enumFromTo     :: a -> a -> [a]
    enumFromThenTo :: a -> a -> a -> [a]

    -- minimal complete definition: toEnum, fromEnum
    
    succ = toEnum . (+1) . fromEnum
    pred = toEnum . (subtract 1) . fromEnum
    enumFrom = map toEnum [fromEnum x ..]
    enumFromTo x y = map toEnum [fromEnum x .. fromEnum y]
    enumFromThen x y = map toEnum [fromEnum x, fromEnum y, ..]
    enumFromThenTo x y z = map toEnum [fromEnum x, fromEnum y .. fromEnum z]
    
class Bounded a where
    minBound :: a
    maxBound :: a
    
-- Numeric Classes

class (Eq a, Show a) => Num a where
    (+), (-), (*) :: a -> a -> a
    negate        :: a -> a
    abs, signum   :: a -> a
    fromInteger   :: Integer -> a
    
    -- minimal complete definition: everything, except negate or (-)
    
    x - y = x + negate y
    negate x = 0 - x
    
class (Num a, Ord a) => Real a where
    toRational :: a -> Rational
    
class (Real a, Enum a) => Integral a where
    quot, rem       :: a -> a -> a
    div, mod        :: a -> a -> a
    quotRem, divMod :: a -> a -> (a,a)
    toInteger       :: a -> Integer
    
    -- minimal complete definition:
    -- quotRem, toInteger
    
    n `quot` d = q where (q, r) = quotRem n d
    n `rem` d  = r where (q, r) = quotRem n d
    n `div` d  = q where (q, r) = divMod n d
    n `mod` d  = r where (q, r) = divMod n d
    divMod n d = if signum r == - signum d then (q - 1, r + d) else qr
                    where qr@(q, r) = quotRem n d
                    

class Num a => Fractional a where
    (/)          :: a -> a -> a
    recip        :: a -> a
    fromRational :: a -> Rational
    
    -- minimal complete definition
    -- fromRational and (recip or (/))
    
    recip x = 1 / x
    x / y = x * recip y
    

class Fractional a => Floating a where
    pi                  :: a
    exp, log, sqrt      :: a -> a
    (**), logBase       :: a -> a -> a
    sin, cos, tan       :: a -> a
    asin, acos, atan    :: a -> a
    sinh, cosh, tanh    :: a -> a
    asinh, acosh, atanh :: a -> a
    
    -- Minimal Complete Definition
    -- pi, exp, log, sin, cos, sinh, cosh, asin, acos, atan,
    -- asinh, acosh, atanh 
    
    x ** y = exp (log x * y)
    logBase x y = log y / log x
    sqrt x = x ** 0.5
    tan x = sin x / cos x
    tanh x = sinh x / cosh x
    
class (Real a, Fractional a) => RealFrac a where
    properFraction :: Integral b => a -> (b, a)
    truncate, round :: Integral b => a -> b
    ceiling, floor  :: Integral b => a -> b
    
    -- minimal complete definition: proper fraction
    
    truncate x = m  where (m, _) = properFraction x
    round x    = let (n,r) = properFraction x
                     m = if r < 0 then n - 1 else n + 1
                   in case signum (abs r - 0.5) of
                            -1 -> n
                             0 -> if even n then n else m
                             1 -> m
    ceiling x = if r > 0 then n + 1 else n
                where (n, r) = properFraction x
    
    floor x = if r < 0 then n - 1 else n
              where (n,r) = properFraction x
    
class (RealFrac a, Floating a) => RealFloat a where
    floatRadix  :: a -> Integer
    floatDigits :: a -> Int 
    floatRange  :: a -> (Int, Int)
    decodeFloat :: a -> (Integer, Int)
    encodeFloat :: Integer -> Int -> a
    exponent    :: a -> Int
    significand :: a -> a 
    scaleFloat  :: Int -> a -> a
    isNaN, isInfinite, isDenormalized, isNegativeZero, 
         isIEEE :: a -> Bool
    atan2 :: a -> a -> a
    -- minimal complete definition
    -- all except exponent, significand, scaleFloat, atan2
    
    exponent x = if m == 0 then 0 else n + floatDigits x
                 where (m, n) = decodeFloat
    significand x = encodeFloat m (- floatDigits x)
                    where (m, _) = decodeFloat x
    scaleFloat k x = encodeFloat m (n + k)
                     where (m, n) = decodeFloat x
                     
    atan2 y x
        | x > 0            = atan (y / x)
        | x == 0 && y > 0  = pi / 2
        | x < 0 && y > 0   = pi + atan (y / x)
        | (x < 0 && y < 0) ||
          (x < 0 && isNegativeZero y) ||
          (isNegativeZero x && isNegativeZero y) = -atan2 (-y) x
                
        | y == 0 && (x < 0 || isNegativeZero x) = pi
        | x == 0 && y == 0 = y
        | otherwise = x + y
        
-- Numeric functions

subtract :: Num a => a -> a -> a
subtract = flip (-)

even, odd :: Integral a => a -> Bool
even n = n `rem` 2 == 0
odd  = not . even



gcd :: Integral a => a -> a -> a
gcd 0 0 = error "Prelude.gcd: gcd 0 0 is undefined"
gcd x y = gcd' (abs x) (abs y)
          where gcd' x 0 = x
                gcd' x y = gcd' y (x `rem` y)


                
lcm :: Integral a => a -> a -> a
lcm _ 0 = 0
lcm 0 _ = 0
lcm x y = abs (x `quot` (gcd x y) * y)



(^) :: (Num a, Integral b) => a -> b -> a
x ^ 0 = 1
x ^ n | n > 0 = f x (n - 1) x
                where
                    f _ 0 y = y
                    f x n y = g x n 
                              where
                                g x n | even n = g (x * x) (n `quot` 2)
                                      | otherwise = f x (n - 1) (x * y)
_ ^ _ = error "Prelude.^: Negative exponent"


fromIntegral :: (Integral a, Num b) => a -> b
fromIntegral = fromInteger . toInteger


realToFrac :: (Real a, Fractional b) => a -> b
realToFrac = fromRational . toRational

-- Monadic classes

class Functor f where
    fmap :: (a -> b) -> f a -> f b
    
class Monad m where
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a -> m b -> m b
    return :: a -> m a
    fail   :: String -> m a
    
    -- Minimal complete definition: (>>=), return
    
    m >> k = m >>= \_ -> k
    fail s = error s
    

sequence :: Monad m => [m a] -> m [a]
sequence = foldr mcons (return [])
           where mcons p q = p >>= \x -> q >>= \y -> return (x:y)
           
sequence_ :: Monad m => [m a] -> m ()
sequence_  = foldr (>>) (return ())

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f as = sequence (map f as)

mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f as = sequence_ (map f as)

(=<<) :: Monad m => (a -> m b) -> m a -> m b
f =<< x = x >>= f


-- Function type

id :: a -> a
id x = x

-- constant function

const :: a -> b -> a
const x _ = x

-- function composition

(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

($), ($!) :: (a -> b) -> a -> b
f $ x  = f x
f $! x = x `seq` f x

-- Boolean type

data Bool = True | False deriving (Eq, Ord, Enum, Read, Show, Bounded)

-- Boolean functions

(&&), (||) :: Bool -> Bool -> Bool 
True && x  = x
False && _ = False

True || _  = True
False || x = x

not :: Bool -> Bool
not True  = False
not False = True

otherwise :: Bool
otherwise = True 

-- character

instance Eq Char where
    c == c' = fromEnum c == fromEnum c'
    
instance Ord Char where
    c <= c' = fromEnum c <= fromEnum c'
    
instance Enum Char where
    toEnum = primIntToChar
    fromEnum = primCharToInt
    enumFrom c = map toEnum [fromEnum c .. fromEnum (maxBound :: Char)]
    enumFromThen c c' = map toEnum [fromEnum c, fromEnum c' .. fromEnum lastChar]
                        where lastChar :: Char
                              lastChar | c' < c = minBound
                                       | otherwise = maxBound
                                       
instance Bounded Char where
    minBound = '\0'
    maxBound = primUnicodeMaxChar
    
type String = [Char]

-- Maybe type

data Maybe a = Nothing | Just a deriving (Eq, Ord, Read, Show)

maybe :: b -> (a -> b) -> Maybe a -> b
maybe n f Nothing = n
maybe n f (Just x) = f x

instance Functor Maybe where
    fmap f Nothing = Nothing 
    fmap f (Just x) = Just (f x)
    
instance Monad Maybe where
    (Just x) >>= k = k x
    Nothing >>= k = Nothing 
    return = Just
    fail s = Nothing 
    
-- Either type

data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f g (Left x) = f x
either f g (Right x) = g x


-- IO data type

instance Functor IO where
    fmap f x = x >>= (return . f)
    
instance Monad IO where
    x >>= f = bindIO x f
    return  = returnIO
    fail s  = ioError (userError s)
    
-- Ordering type

data Ordering = LT | EQ | GT deriving (Eq, Ord, Read, Enum, Show, Bounded)

-- standard numeric types. 

instance Eq Int where
    x == y = primEqInt x y
    
instance Ord Int where
    x <= y = primLTEInt x y
    
instance Num Int where
    x + y = primPlusInt x y
    x - y = primMinusInt x y
    x * y = primMultInt x y
    abs x = if x < 0 then (-1) * x else x
    signum x 
        | x < 0  = -1
        | x == 0 = 0
        | x > 0  = 1
    fromInteger = primIntegerToInt
    
instance Real Int where

instance Integral Int where

instance Enum Int where

instance Bound Int where
    
instance Eq Integer where
    x == y = primEqInteger x y
    
instance Ord Integer where
    x <= y = primLTEInteger x y

instance Num Integer where
    x + y = primPlusInteger x y
    x - y = primMinusInteger x y
    x * y = primMultInteger x y
    abs x = if x < 0 then (-1) * x else x
    signum x 
        | x < 0  = -1
        | x == 0 = 0
        | x > 0  = 1
    fromInteger = id



    
    
