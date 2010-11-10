> {-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

This module defines a operation to get names from things
First we define a type class just to say "this type is a name", and
from this we define another class to the operation of get a name.

> module Tc.Utils.Nameable where

> import Control.Monad
> import Data.Generics hiding (TyCon)
> import Data.List (union)

> import Debug.Trace

> import Syntax.CoreSyn
> import Tc.Class
> import Tc.Assumption

> import Utils.Pretty

A data type to define the kind of dependency analysis need to be done

> data DependencyTy = KindAnalysis | TypeAnalysis deriving (Eq, Ord, Show)

The class to define that a type is a name

> class IsNameTy a 

> instance IsNameTy Name

The operation of getting a name

> class IsNameTy b => HasName a b | a -> b where
>    getName            :: DependencyTy -> a b -> b
>    getReferencedNames :: a b -> [b] -> [b]

> instance HasName Assumption Name where
>    getName TypeAnalysis (n :>: _)    = n
>    getName KindAnalysis (_ :>: Forall (_ :=> t))  = tyConNameFrom t
>    getReferencedNames   (_ :>: sch) ns = namesFrom ns sch
>                                        
>                                           

> instance HasName Bind Name where
>    getName _ (FunBind n _ _) = n
>    getName _ (PatBind p _ _)   = Unqual (show $ pprint p)

>    getReferencedNames (FunBind _ ty as) ns = (concatMap (namesFrom ns) (rmMaybe as))
>                                                 `union` (namesFrom ns ty)
>    getReferencedNames (PatBind _ _ e) ns = namesFrom ns e

> instance HasName Class Name where
>    getName _ c = name c

>    getReferencedNames (Class _ _ sups mem _) ns = (concatMap (namesFrom ns) sups)
>                                                             `union` (concatMap (namesFrom ns) mem)
>                                                   


> instance HasName Inst Name where
>    getName _ i = instname i

>    getReferencedNames _ _ = []

> instance HasName TyCon Name where
>    getName _ (TyCon n _) = n
>    getReferencedNames _ _ = []




> tyConNameFrom (TyFun _ r)   = tyConNameFrom r
> tyConNameFrom (TCon (TyCon n _))   = n
> tyConNameFrom t@(TyApp l _) = tyConNameFrom l
> tyConNameFrom (TVar (Bound n _))   = n

> namesFrom ns x = listify (flip elem ns) x

> rmMaybe :: Maybe [a] -> [a]
> rmMaybe (Just xs) = xs
> rmMaybe Nothing   = []
