> {-# LANGUAGE DeriveDataTypeable #-}

Here is the definition of the syntax for literals.

> module Syntax.LitSyn where

> import Data.Foldable
> import Data.Generics

> import Numeric(fromRat) -- to convert a rational to float

> import Utils.Pretty

the data definition. We parametrize the types of the AST by the type of names. This is util por implement
a renamer.

> data Lit a = LitInt Integer
>            | LitRat Rational
>            | LitChar Char
>            | LitStr String  -- I prefer to not desugar string literals. But it is easy to implement.
>            deriving (Eq, Ord, Show, Data, Typeable)

the printer

> instance Pretty a => Pretty (Lit a) where
>     pprint (LitInt  i) = integer i
>     pprint (LitRat  r) = float $ fromRat r
>     pprint (LitChar c) = char c
>     pprint (LitStr  s) = text s
