In this module we colect all classes / instances to lift their respective binds
to top level.

To do these we gen a name for these class defaults and instance methods, and let
it type check like normal binds.

> module Desugar.LiftClassInstBinds(desugarClassInstDefs, OverloadBinds (..)) where

> import Control.Monad
> import Control.Monad.Trans
> import Data.List (partition)
> import Haskell.Syntax.Syntax

> import Desugar.Desugar
> import Desugar.DesugarDecl
> import Desugar.DesugarName
> import Desugar.DesugarTy hiding (desugar')

> import Syntax.CoreSyn

> import Tc.Assumption
> import Tc.Class
> import Utils.Pretty

The driver function

> desugarClassInstDefs :: HsModule -> DesugarM ([Class Name], OverloadBinds)
> desugarClassInstDefs m = do
>                            (cs,bs)  <- liftM unzip (liftClassMethods m)
>                            (is,bs') <- liftM unzip (liftInstMethods m)
>                            return (map (add is) cs, OverloadBinds (zip cs bs) (zip is bs'))

Lifting class methods (default implementations) and getting its signatures
to build \Gamma^{cls}

> liftClassMethods :: HsModule -> DesugarM [(Class Name, Binds Name)]
> liftClassMethods (HsModule loc m exps imps decls) 
>                       = do
>                            let
>                               cls = filter isClass decls
>                            mapM processClassDef cls

The class value doesnt have (yet) the associated instances.

> processClassDef :: HsDecl -> DesugarM (Class Name, Binds Name)
> processClassDef (HsClassDecl _ ctxt n vs decls) 
>                       = do
>                           let
>                               (binds, sigs)   = partition isBind decls 
>                           binds' <- mapM desugarHsDecl binds     
>                           n'     <- desugarHsName n
>                           vs'    <- mapM (liftM (flip Bound (KVar (KindVar (-1)))) . desugarHsName) vs  
>                           sigs'  <- mapM (liftM (toAssump n' vs') . desugarHsDecl) sigs
>                           ctxt'  <- mapM desugar' ctxt
>                           return (Class n' vs' ctxt' sigs' [], binds')


Lifting instance methods and building data types to represent instance constraints

> liftInstMethods :: HsModule -> DesugarM [(Inst Name, Binds Name)]
> liftInstMethods (HsModule loc m exps imps decls) 
>                           = do
>                                 let
>                                   is = filter isInst decls
>                                 mapM processInstDef is


> processInstDef :: HsDecl -> DesugarM (Inst Name, Binds Name)                                 
> processInstDef (HsInstDecl _ ctxt n ts decls) 
>                           = do
>                               ctxt'  <- mapM desugar1 ctxt
>                               n'     <- desugarHsQName n
>                               ts'    <- mapM desugarHsType ts
>                               decls' <- mapM desugarHsDecl decls
>                               return (Inst n' ts' ctxt', decls')

Some auxiliar functions

> add is c = c{ instances = [i | i <- is, (instname i) == (name c)]}

> unvar (TVar v)  = v :: TyVar Name

> desugar' (n,ts) = liftM2 ((,)) (desugarHsQName n) 
>                                (mapM (liftM unvar . desugarHsType) ts)


> desugar1 (n,ts) = liftM2 ((,)) (desugarHsQName n) 
>                                (mapM desugarHsType ts)


> toAssump :: Name -> [TyVar Name] -> Bind Name -> Assumption Name
> toAssump n ns (FunBind n' (Just (Forall (qs :=> ty))) Nothing) 
>                   = (n' :>: sch)
>                      where
>                         sch = Forall (qs' :=> ty)
>                         qs' = (n, map TVar ns) : qs 

> isClass (HsClassDecl _ _ _ _ _) = True
> isClass _                       = False

> isInst (HsInstDecl _ _ _ _ _) = True
> isInst _                      = False

> isBind (HsFunBind _)       = True
> isBind (HsPatBind _ _ _ _) = False
> isBind _                   = False
