> {-#LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

This module contains the functions for type instantiation

> module Tc.TcInst where

> import qualified Data.Map as Map

> import Syntax.CoreSyn


> type InstMap = Map.Map (TyVar Name) (Ty Name)

> class Instantiate t where
>    inst :: InstMap -> t -> t


> instance Instantiate t => Instantiate [t] where
>    inst m = map (inst m)


> instance Instantiate (Ty Name) where
>    inst m (TVar t)     = case Map.lookup t m of
>                                Just ty -> ty
>                                Nothing -> TVar t
>    inst m (TyApp l r)  = TyApp (inst m l) (inst m r)
>    inst m (TyFun l r)  = TyFun (inst m l) (inst m r)
>    inst m (TyTuple ts) = TyTuple (inst m ts)
>    inst m (TyList t)   = TyList (inst m t)
>    inst m t            = t

> instance Instantiate (Pred Name) where
>    inst m (n,ts) = (n, inst m ts)

> instance Instantiate (Qual Name) where
>    inst m (ps :=> t) = (inst m ps) :=> (inst m t)

> instance Instantiate (Scheme Name) where
>    inst m (Forall qt) = Forall (inst m qt)

> instantiate :: Instantiate t => [TyVar Name] -> [Ty Name] -> t -> t
> instantiate vs ts = inst tymap
>                     where
>                       tymap = Map.fromList (zip vs ts)
