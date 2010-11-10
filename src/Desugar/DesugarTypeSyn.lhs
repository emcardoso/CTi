This file contains the algorithm for type synonym 
expansion.

> module Desugar.DesugarTypeSyn (expandTySynonyms) where

> import Haskell.Syntax.Syntax
> import Haskell.Pretty.Pretty(prettyPrint)
> import Data.List (partition)
> import Data.Generics (everywhereM, mkM)

> import Debug.Trace

> import Desugar.Desugar

First, the definition of a monad for the expasion algorithm, note that
we use the SYB... Man... its cool! ;-)


> expandTySynonyms :: HsModule -> DesugarM HsModule
> expandTySynonyms (HsModule l m e i decls) = do
>                                               let 
>                                                 (ts, ds) = partition isTypeSyn decls
>                                               modifyTypeSynEnv (mkTySynEnv ts)
>                                               decls' <- everywhereM (mkM expand) ds
>                                               return (HsModule l m e i (ts ++ decls'))

> expand :: HsType -> DesugarM HsType
> expand t = do
>             ty <- lookupTySyn t
>             return $ case ty of
>                        Nothing  -> t
>                        Just ty' -> ty'


> isTypeSyn (HsTypeDecl _ _ _ _) = True
> isTypeSyn _                    = False

> mkTySynEnv = map go 
>              where
>                 go (HsTypeDecl loc n ns ty) = (foldl HsTyApp (HsTyCon (UnQual n)) (map HsTyVar ns), ty)
