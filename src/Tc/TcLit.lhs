This module does the type checking / inference of literals

> module Tc.TcLit where

> import Syntax.CoreSyn

> import Tc.TcMonad
> import Prelude.PreludeBuiltIn

> tcLit :: Lit Name -> TcM (Ty Name)
> tcLit (LitInt _)  = return tInteger
> tcLit (LitRat _)  = return tRational
> tcLit (LitChar _) = return tChar
> tcLit (LitStr _)  = return tString
