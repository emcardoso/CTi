> {-# LANGUAGE TypeSynonymInstances, DeriveDataTypeable #-}

This file contains the whole syntax of expressions.

> module Syntax.ExprSyn where

> import Data.Generics

> import Syntax.LitSyn 
> import Syntax.PatSyn
> import Syntax.BindSyn

> import Utils.Pretty

Data type of expressions. Expressions are just a lambda-calculus with constants, let e case expressions.

> type Alt a = ([Pat a], Expr a)

> type Alts a = [Alt a]

> data Expr a = Var a
>             | Con a
>             | Const (Lit a)
>             | Lam [Pat a] (Expr a)
>             | App (Expr a) (Expr a)
>             | Case (Expr a) (Alts a)
>             | If (Expr a) (Expr a) (Expr a) -- Again this is easy to translate after
>             | List [Expr a]
>             | Tuple [Expr a]
>             | Let (Binds a) (Expr a)
>             deriving (Eq, Ord, Show, Data, Typeable)

The printer

> instance Pretty a => Pretty (Expr a) where
>     pprint (Var v) = pprint v
>     pprint (Con v) = pprint v
>     pprint (Const l) = pprint l
>     pprint (Lam vs e) = plam <> parens (pUnwords vs) <+> parrow <+> pprint e
>     pprint (App e1 e2) = pprint e1 <+> pprint e2
>     pprint (Case e as) = hsep [pcase, pprint e, pof] $$ nest ident (pprint as)
>     pprint (If e1 e2 e3) = hsep [pif, pprint e1, pthen, pprint e2, pelse, pprint e3]
>     pprint (List es) = brackets (pPunctuate comma es)
>     pprint (Tuple es) = parens (pPunctuate comma es)
>     pprint (Let ds e) = plet $$ nest ident (pUnlines ds) $$ pin <+> pprint e

> instance Pretty a => Pretty (Alt a) where
>     pprint (ps, e) = parens (pUnwords ps) <+> peq <+> pprint e

> instance Pretty a => Pretty (Alts a) where
>     pprint = pUnlines 
