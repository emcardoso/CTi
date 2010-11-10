this file collects the types of data constructors defined in current
module. In this module we create the derived instance constraints for
the data types defined in the module.

> module Desugar.CollectDataCons (collectDataCons) where

> import Control.Monad

> import Haskell.Syntax.Syntax

> import Desugar.Desugar
> import Desugar.DesugarName
> import Desugar.DesugarTy
> import Desugar.Deriving

> import Syntax.CoreSyn

> import Tc.Assumption
> import Tc.Class


> collectDataCons :: HsModule -> DesugarM ([Assumption Name], [Inst Name])
> collectDataCons (HsModule _ _ _ _ decls) 
>                       = do
>                           let
>                               ds = filter isDataDecl decls
>                           as <- liftM concat (mapM toAssump ds)
>                           is <- liftM concat (mapM deriveInsts ds)
>                           return (as, is)


> toAssump :: HsDecl -> DesugarM [Assumption Name]
> toAssump (HsDataDecl _ ctxt n vs cons ders)   
>           = do
>                ctxt' <- mapM desugar' ctxt
>                tc    <- mkTyCon n vs
>                mapM (createDataCon tc ctxt') cons
> toAssump (HsNewTypeDecl _ ctxt n vs con ders) 
>           = do
>               ctxt' <- mapM desugar' ctxt
>               tc    <- mkTyCon n vs
>               mapM (createDataCon tc ctxt') [con]

> createDataCon :: Ty Name -> [Pred Name] -> HsConDecl -> DesugarM (Assumption Name)
> createDataCon ty ctxt (HsConDecl _ n tys) =  do
>                                               n'   <- desugarHsName n
>                                               tys' <- mapM (desugarHsType . unBangTy) tys
>                                               let ty' = foldr TyFun ty tys'
>                                               return (n' :>: (Forall (ctxt :=> ty')))
> createDataCon _  _ (HsRecDecl _ _ _) = throwError "Records aren't supported yet!\nPlease use conventional data types"

some auxiliar functions

> unBangTy (HsBangedTy t)   = t
> unBangTy (HsUnBangedTy t) = t

> isDataDecl (HsDataDecl _ _ _ _ _ _)    = True
> isDataDecl (HsNewTypeDecl _ _ _ _ _ _) = True
> isDataDecl _                           = False

