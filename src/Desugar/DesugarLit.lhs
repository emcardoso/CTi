This file aims to do the desugaring of literals. 
Here we follow the definition of the Haskell report.

> module Desugar.DesugarLit where

> import Haskell.Syntax.Syntax

> import Desugar.Desugar

> import Syntax.LitSyn
> import Syntax.NameSyn

Here comes the definition of the desugaring of literals

> desugarHsLiteral :: HsLiteral -> DesugarM (Lit Name)
> desugarHsLiteral (HsInt i)    = return $ LitInt i
> desugarHsLiteral (HsChar c)   = return $ LitChar c
> desugarHsLiteral (HsFrac r)   = return $ LitRat r
> desugarHsLiteral (HsString s) = return $ LitStr s
