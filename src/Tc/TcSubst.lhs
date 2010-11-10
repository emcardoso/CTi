> {-#LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

This file defines type substitutions and operations over it.

> module Tc.TcSubst where

> import Data.List

> import Syntax.CoreSyn
> import Tc.Assumption


The definition of a substitution

> type TySubst a = [(TyVar a, Ty a)]

> nullSubst :: TySubst a
> nullSubst = []


a type class for values that can be substitutable

> class Substitutable t where
>       apply :: (TySubst Name) -> t -> t
>       tv    :: t -> [TyVar Name]

Getting the bound and free variables of a type

> bv :: (Substitutable t) => t -> [TyVar Name]
> bv = filter isBound . tv

> fv :: (Substitutable t) => t -> [TyVar Name]
> fv = filter (not . isBound) . tv

> instance  Substitutable t => Substitutable [t] where
>       apply s = map (apply s)
>       tv      = nub . concatMap tv

> instance Substitutable (Ty Name) where
>       apply s (TVar v)     = case lookup v s of
>                                  Just t  -> t
>                                  Nothing -> TVar v
>       apply s (TyApp l r)  = TyApp (apply s l) (apply s r)
>       apply s (TyFun l r)  = TyFun (apply s l) (apply s r)
>       apply s (TyTuple ts) = TyTuple (apply s ts)
>       apply s (TyList t)   = TyList (apply s t)
>       apply s (TyAnd ts)   = TyAnd (apply s ts) -- ALT:
>       apply s t            = t

>       tv (TVar v)     = [v]
>       tv (TyApp l r)  = tv l `union` tv r
>       tv (TyFun l r)  = tv l `union` tv r
>       tv (TyTuple ts) = foldr (union . tv) [] ts
>       tv (TyAnd ts)   = foldr (union . tv) [] ts  -- ALT:
>       tv (TyList t)   = tv t
>       tv t            = []

> instance Substitutable (Pred Name) where
>       apply s (n,t) = (n, apply s t)
>       tv (_, t)     = tv t

> instance Substitutable (Qual Name) where
>       apply s (ps :=> t) = (apply s ps) :=> (apply s t)
>       tv (ps :=> t)      = tv ps `union` tv t

> instance Substitutable (Scheme Name) where
>       apply s (Forall qt) = Forall (apply s qt)
>       tv (Forall qt)      = tv qt

> instance Substitutable (Assumption Name) where
>       apply s (i :>: sc) = i :>: (apply s sc)
>       tv (i :>: sc)      = tv sc

Substitution composition

> infixr 4 @@

> (@@) :: TySubst Name -> TySubst Name -> TySubst Name
> s1 @@ s2 = [(u, apply s1 t) | (u,t) <- s2] ++ s1


Merging a substitution

> merge :: Monad m => TySubst Name -> TySubst Name -> m (TySubst Name)
> merge s1 s2 = if agree then return (s1 ++ s2) else fail "Cannot merge the substitutions"
>               where agree = all (\v -> apply s1 (TVar v) == apply s2 (TVar v))
>                                 (map fst s1 `intersect` map fst s2)


> isBound (Bound _ _) = True
> isBound _           = False
