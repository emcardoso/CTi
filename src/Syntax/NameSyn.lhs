> {-# LANGUAGE DeriveDataTypeable #-}


This file contains the definition of types for names. Names can be
qualified and non-qualified. The type will support both. When I
implement the renamer, the name must converted to another data type
that allows only qualified names.

> module Syntax.NameSyn where

> import Data.Generics

> import Utils.Pretty


the data type definition. We dont need to specify what will be operator
or what will be a ident. This is a task for the parser.

> type Ident = String

> data Name  = Qual Ident Ident
>            | Unqual Ident 
>            | ListCon
>            | TupleCon Int
>            | ArrowCon
>            | ConsCon
>            deriving (Ord, Show, Data, Typeable)

> instance Eq Name where
>   (Qual m n) == (Qual m' n')     = m == m' && n == n'
>   (Unqual m) == (Unqual m')      = unrename m == unrename m'
>   ListCon == ListCon             = True
>   (TupleCon n)  == (TupleCon n') = n == n'
>   ArrowCon == ArrowCon           = True
>   ConsCon == ConsCon             = True
>   _ == _                         = False

> unrename = takeWhile (/= '_')

the pretty printer

> instance Pretty Name where
>     pprint (Qual m n)   = text m <> pdot <> text n
>     pprint (Unqual n)   = text $ unrename n
>     pprint (TupleCon n) = parens (text (replicate n ','))
>     pprint ListCon      = text "[]"
>     pprint ArrowCon     = text "->"
>     pprint ConsCon      = char ':'
