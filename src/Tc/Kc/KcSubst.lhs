This module defines the substitution over kinds

> module Tc.Kc.KcSubst where

> import Data.List (union)

> import Syntax.CoreSyn


Definition of overloaded operations over kinds

> class Kinds a where
>    vars  :: a -> [KindVar]
>    apply :: KindSubst -> a -> a

> instance Kinds Kind where
>    vars Star         = []
>    vars (KVar v)     = [v]
>    vars (KFun k1 k2) = vars k1 `union` vars k2

>    apply s Star         = Star
>    apply s (KFun k1 k2) = KFun (apply s k1) (apply s k2)
>    apply s (KVar k)     = case lookup k s of
>                                Just k' -> k'
>                                Nothing -> KVar k

> instance Kinds a => Kinds [a] where
>     vars    = foldr (union . vars) []
>     apply s = map (apply s)


Definition of kind substitution

> type KindSubst = [(KindVar, Kind)]

> nullSubst :: KindSubst
> nullSubst = []

Substitution operations

> (@@) :: KindSubst -> KindSubst -> KindSubst 
> s1 @@ s2 = [(u, apply s1 k) | (u, k) <- s2] ++ s1
