This file is responsible for the desugaring
of declarations. Here we just desugar the bindings
in top level and class / instance declarations

> module Desugar.DesugarDecl where

> import Control.Monad
> import Haskell.Syntax.Syntax

> import Syntax.BindSyn
> import Syntax.NameSyn
> import Syntax.ExprSyn
> import Syntax.LitSyn

> import Prelude.PreludeBuiltIn

> import {-# SOURCE #-} Desugar.DesugarExpr
> import Desugar.DesugarPat
> import Desugar.DesugarName
> import Desugar.DesugarTy
> import Desugar.Desugar

> desugarHsDecl :: HsDecl -> DesugarM (Bind Name)
> desugarHsDecl (HsTypeSig _ [n] qt)   = do
>                                           qt' <- desugarHsQualType qt
>                                           n'   <- desugarHsName n
>                                     	    return (FunBind n' (Just qt') Nothing)
> desugarHsDecl (HsFunBind ms)         = liftM toFunBind (mapM desugarHsMatch ms) 
> desugarHsDecl (HsPatBind _ p rhs ds) = do
>                                           ([p'], e) <- desugarMatch [p] rhs ds 
>                                           return (PatBind p' Nothing e)
> desugarHsDecl _ = throwError "Panic! desugarHsDecl!"

> desugarHsMatch :: HsMatch -> DesugarM (Name, Alt Name)
> desugarHsMatch (HsMatch _ n ps rhs ds) = liftM2 ((,)) (desugarHsName n) (desugarMatch ps rhs ds)

> desugarHsRhs :: HsRhs -> DesugarM (Expr Name)
> desugarHsRhs (HsUnGuardedRhs e) = desugarHsExp e
> desugarHsRhs (HsGuardedRhss gs) = mapM desugarHsGuardedRhs gs >>= \gs' -> return (result gs')
>                                    where
>                                       ifthenelse (g,e) ac = If g e ac
>                                       errPatFail = App errorSyn (Const (LitStr "Unmatched pattern!"))
>                                       result = foldr ifthenelse errPatFail 

> desugarHsGuardedRhs :: HsGuardedRhs -> DesugarM (Expr Name, Expr Name)
> desugarHsGuardedRhs (HsGuardedRhs _ e1 e2) = liftM2 ((,)) (desugarHsExp e1) (desugarHsExp e2)

> desugarMatch :: [HsPat] -> HsRhs -> [HsDecl] -> DesugarM (Alt Name)
> desugarMatch ps rhs ds = do
>                            ps'  <- mapM desugarHsPat ps
>                            rhs' <- desugarHsRhs rhs
>                            ds'  <- mapM desugarHsDecl ds
>                            return $ case ds' of
>                                        []    -> (ps', rhs')
>                                        (_:_) -> (ps', Let ds' rhs')

> toFunBind :: [(Name, Alt Name)] -> Bind Name
> toFunBind ms = let ((n:_), as) = unzip ms
>                 in (FunBind n Nothing (Just as))
