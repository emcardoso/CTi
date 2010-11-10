This file contains the definition of the pattern desugar.

> module Desugar.DesugarPat where

> import Control.Monad

> import Desugar.Desugar
> import Desugar.DesugarName
> import Desugar.DesugarLit

> import Prelude.PreludeBuiltIn

> import Syntax.PatSyn
> import Syntax.NameSyn


> import Haskell.Syntax.Syntax

Here comes the desugar of patterns

> desugarHsPat :: HsPat -> DesugarM (Pat Name)
> desugarHsPat (HsPVar n) = liftM PVar (desugarHsName n)
> desugarHsPat (HsPLit l) = liftM PLit (desugarHsLiteral l)
> desugarHsPat (HsPNeg p) = liftM (PApp negName) (mapM desugarHsPat [p])
> desugarHsPat (HsPInfixApp p1 op p2) = desugarPInfix op p1 p2
> desugarHsPat (HsPApp qn ps) = liftM2 PApp (desugarHsQName qn) (mapM desugarHsPat ps)
> desugarHsPat (HsPTuple ps) = liftM PTuple (mapM desugarHsPat ps)
> desugarHsPat (HsPList ps)  = liftM PList (mapM desugarHsPat ps)
> desugarHsPat (HsPRec _ _)  = throwError "Records arent supported! Use convetional data types!"
> desugarHsPat (HsPAsPat n p)  = liftM2 PAs (desugarHsName n) (desugarHsPat p)
> desugarHsPat HsPWildCard = return PWildCard
> desugarHsPat (HsPIrrPat p) = liftM PIr (desugarHsPat p)
> desugarHsPat (HsPParen p) = desugarHsPat p

> desugarPInfix :: HsQName -> HsPat -> HsPat -> DesugarM (Pat Name)
> desugarPInfix op p1 p2 = do
>                           op' <- desugarHsQName op
>                           ps  <- mapM desugarHsPat [p1, p2]
>                           return (PApp op' ps)
