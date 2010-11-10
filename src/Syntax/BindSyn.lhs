> {-# LANGUAGE TypeSynonymInstances, DeriveDataTypeable #-}

this file contains the definitions of function bindings.

> module Syntax.BindSyn where

> import Data.Generics

> import {-# SOURCE #-} Syntax.ExprSyn
> import Syntax.PatSyn
> import Syntax.TySyn

> import Utils.Pretty

Bindings are formed by an expression and a identifier. 
Basically binds can be implict and explicit typed. 
A explicit typed bind has the shape (Jus sch) for a
type scheme sch.

Binding are used to represent type signatures. 
A type signature is represented by (id, Just sch, Nothing)

> data Bind a = FunBind a (Maybe (Scheme a)) (Maybe (Alts a)) 
>             | PatBind (Pat a) (Maybe (Scheme a)) (Expr a) 
>               deriving (Eq, Ord, Show, Data, Typeable)

> type Binds a = [Bind a]

Printer

> instance Pretty a => Pretty (Bind a) where
>     pprint (FunBind i  Nothing  (Just e))  = pprint i <+> peq <+> pprint e
>     pprint (FunBind i (Just sch) (Just e)) = pprint i <+> pprint sch $$ pprint i <+> peq <+> pprint e
>     pprint (FunBind i (Just sch) Nothing)  = pprint i <+> text "::" <+> pprint sch
>     pprint (PatBind p (Just sch) e)        = pprint p <+> text "::" <+> pprint sch $$ pprint p <+> peq <+> pprint e
>     pprint (PatBind p _ e)                 = pprint p <+> peq <+> pprint e
>     pprint _                               = error "Panic!\nInvalid declaration!"

> instance Pretty a => Pretty (Binds a) where
>     pprint = pUnlines
