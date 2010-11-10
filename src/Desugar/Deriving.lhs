This file contains the definitions of the deriving mechanism.
Here we just create the equivalent constraint.

> module Desugar.Deriving where

> import Haskell.Syntax.Syntax

> import Desugar.Desugar
> import Desugar.DesugarName
> import Desugar.DesugarTy

> import Syntax.CoreSyn

> import Tc.Class

> deriveInsts :: HsDecl -> DesugarM [Inst Name]
> deriveInsts (HsDataDecl _ ctxt n vs _ ders) 
>                   = do
>                       ctxt' <- mapM desugar' ctxt
>                       ty    <- mkTyCon n vs
>                       mapM (genInst ctxt' ty) ders
> deriveInsts (HsNewTypeDecl _ ctxt n vs _ ders)  
>                   = do
>                       ctxt' <- mapM desugar' ctxt
>                       ty    <- mkTyCon n vs
>                       mapM (genInst ctxt' ty) ders


> genInst :: [Pred Name] -> Ty Name -> HsQName -> DesugarM (Inst Name)
> genInst ps ty n = do
>                       n' <- desugarHsQName n
>                       return (Inst n' [ty] ps)


> mkTyCon :: HsName -> [HsName] -> DesugarM (Ty Name)
> mkTyCon n vs = do
>                   n'  <- desugarHsName n
>                   vs' <- mapM desugarHsName vs    
>                   let vs'' = map (TVar . flip Bound k) vs'
>                   return (foldl TyApp (TCon $ TyCon n' k) vs'')


Default kind before the kind inference.

> k = KVar (KindVar (-1))
