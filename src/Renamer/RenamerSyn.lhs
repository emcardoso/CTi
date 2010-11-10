This module aims to do a simple renaming of type variables.

> module Renamer.RenamerSyn where

> import Control.Monad
> import Control.Monad.Error
> import Data.List (partition)
> import Haskell.Syntax.Syntax
> import Haskell.Pretty.Pretty(prettyPrint)

> import Renamer.Renamer


here comes the renamer

> renamer :: HsModule -> IO (Either String HsModule)
> renamer m = liftM fst $ runRenamerM (renamerSyntax m)

> renamerSyntax :: HsModule -> RenamerM HsModule
> renamerSyntax (HsModule l m exp imp ds) = liftM (HsModule l m exp imp) 
>                                                 (mapM (blockScope . renamerHsDecl) ds)

> renamerHsDecl :: HsDecl -> RenamerM HsDecl
> renamerHsDecl (HsTypeDecl l n ns ty) = do
>                                           ns' <- putInEnv ns
>                                           ty'  <- renamerHsType True ty
>                                           return (HsTypeDecl l n ns' ty')
> renamerHsDecl (HsDataDecl l ctx n ns cs ds) 
>                   = do
>                       ns'  <- putInEnv ns
>                       ctx' <- mapM renamerHsAsst ctx
>                       cs'  <- mapM renamerDataCon cs
>                       return (HsDataDecl l ctx' n ns' cs' ds)
> renamerHsDecl (HsNewTypeDecl l ctx n ns c ds)
>                   = do
>                       ns' <- putInEnv ns
>                       ctx' <- mapM renamerHsAsst ctx
>                       c'  <- renamerDataCon c
>                       return (HsNewTypeDecl l ctx' n ns' c' ds)
> renamerHsDecl (HsClassDecl l ctx n ns ds)
>                   = do
>                       ns'  <- putInEnv ns
>                       ctx' <- mapM renamerHsAsst ctx
>                       ds'  <- mapM renamerHsDecl ds
>                       return (HsClassDecl l ctx' n ns' ds') 
> renamerHsDecl (HsInstDecl l ctx n ts ds)
>                   = do
>                       let isVar (HsTyVar _) = True
>                           isVar _           = False
>                           unVar (HsTyVar n) = n
>                           (vs,ts')          = partition isVar ts
>                       ns'  <- putInEnv (map unVar vs)
>                       ctx' <- mapM renamerHsAsst ctx
>                       ts'' <- mapM (renamerHsType True) ts'
>                       return (HsInstDecl l ctx' n (map HsTyVar ns' ++ ts'') ds)
> renamerHsDecl (HsTypeSig l ns (HsQualType ctx ty)) 
>                   = do
>                       ctx' <- mapM renamerHsAsst ctx
>                       ty'  <- renamerHsType False ty
>                       return (HsTypeSig l ns (HsQualType ctx' ty'))
> renamerHsDecl t = return t

> renamerHsAsst :: HsAsst -> RenamerM HsAsst
> renamerHsAsst (n,ts) = liftM ((,) n) (mapM (renamerHsType False) ts)


> renamerDataCon :: HsConDecl -> RenamerM HsConDecl
> renamerDataCon (HsRecDecl _ _ _) = throwError "Records declarations aren't supported!"
> renamerDataCon (HsConDecl l n ts) = liftM (HsConDecl l n . map HsUnBangedTy) 
>                                                 (mapM (renamerHsType True . unBang) ts)
>                                              where
>                                                   unBang (HsBangedTy t)   = t
>                                                   unBang (HsUnBangedTy t) = t


> renamerHsType :: Bool -> HsType -> RenamerM HsType
> renamerHsType varExist (HsTyVar n)    = do
>                                           n' <- lookupEnv varExist n 
>                                           return (HsTyVar (HsIdent (prettyPrint n ++ "_" ++ show n')))
> renamerHsType v (HsTyTuple ts) = liftM HsTyTuple (mapM (renamerHsType v) ts)
> renamerHsType v (HsTyFun l r)  = liftM2 HsTyFun (renamerHsType v l) (renamerHsType v r)
> renamerHsType v (HsTyApp l r)  = liftM2 HsTyApp (renamerHsType v l) (renamerHsType v r)
> renamerHsType _ t = return t
