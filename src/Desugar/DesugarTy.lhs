This file does the desugar of type expressions.

> module Desugar.DesugarTy where

> import Haskell.Syntax.Syntax

> import Control.Monad
> import Syntax.CoreSyn

> import Desugar.Desugar
> import Desugar.DesugarName


> desugarHsQualType :: HsQualType -> DesugarM (Scheme Name)
> desugarHsQualType (HsQualType ctxt ty) = do
>                                            ty' <- desugarHsType ty
>                                            ctxt' <- mapM desugar' ctxt
>                                            return (Forall (ctxt' :=> ty'))


> desugarHsType :: HsType -> DesugarM (Ty Name)
> desugarHsType (HsTyFun f a)  = liftM2 TyFun (desugarHsType f) (desugarHsType a)
> desugarHsType (HsTyTuple ps) = liftM TyTuple (mapM desugarHsType ps)
> desugarHsType (HsTyApp l r)  
>                       | l == list_tycon = liftM TyList (desugarHsType r)
>                       | otherwise       = liftM2 TyApp (desugarHsType l) (desugarHsType r)
> desugarHsType (HsTyVar n)    = liftM (TVar . flip Bound (KVar (KindVar (-1)))) (desugarHsName n)
> desugarHsType (HsTyCon qn)   = liftM (TCon . flip TyCon (KVar (KindVar (-1)))) (desugarHsQName qn)
> desugarHsType (HsTyAnd lt)   = liftM TyAnd (mapM desugarHsType lt) -- ALT: desugar intersection Types 
>

> desugar' (n,ts) = liftM2 ((,)) (desugarHsQName n) 
>                                (mapM desugarHsType ts)
