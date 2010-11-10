> {-# LANGUAGE DeriveDataTypeable #-}

The definition of pattern syntax.

> module Syntax.PatSyn where

> import Data.Generics

> import Syntax.LitSyn

> import Utils.Pretty

data type definition

> data Pat a = PVar a
>            | PWildCard
>            | PAs a (Pat a)
>            | PLit (Lit a)
>            | PApp a [Pat a]    -- constructors and its patterns
>            | PIr (Pat a)
>            | PList [Pat a]     -- again its just to easy to checking. The desugar is simple!
>            | PTuple [Pat a]    -- PTuple [] stands for ()
>            deriving (Eq, Ord, Show, Data, Typeable)


The printer

> instance Pretty a => Pretty (Pat a) where
>     pprint (PVar v)  = pprint v
>     pprint PWildCard = text "_"
>     pprint (PAs v p) = pprint v <> pat <> parens (pprint p)
>     pprint (PLit l)  = pprint l
>     pprint (PApp v ps) = parens (pprint v <+> pUnwords ps)
>     pprint (PList ps)  = brackets (pPunctuate comma ps)
>     pprint (PTuple ps) = parens (pPunctuate comma ps)
