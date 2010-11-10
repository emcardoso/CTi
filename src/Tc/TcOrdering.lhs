> {-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

This module defines the type ordering between types, set of types and
typing contexts.

> module Tc.TcOrdering where

> import Control.Monad.Error

> import Syntax.CoreSyn

> import Tc.TcMonad
> import Tc.TcSubst
> import Tc.TcUnify

> import Utils.Pretty

This type class defines the ordering between types.

> class TyOrd t where
>    (<:) :: t -> t -> TcM Bool


> instance (MGU t, Substitutable t, TyOrd t) => TyOrd [t] where
>    ts1 <: ts2 = leq ts1 ts2 `catchError` (const $ return False)
>                 where
>                     leq t1 t2 = do { leq1 t1 t2 ; return True }
>                     leq1 [] t2 = return nullSubst
>                     leq1 (t:ts) (t':ts') = do
>                                               s  <- unifyOrd t t'
>                                               s' <- leq1 (apply s ts) 
>                                                          (apply s ts')
>                                               return (s' @@ s)


> instance TyOrd (Ty Name) where
>    t1 <: t2 = do { unifyOrd t1 t2 ; return True } 
>                   `catchError` (const $ return False)                  


> instance TyOrd (Pred Name) where
>   p1@(n,ts) <: p2@(n',ts') 
>           | n == n'   = ts <: ts'
>           | otherwise = throwError (concat ["Cannot compare the following constraints:",
>                                              show $ pprint p2, "\n", show $ pprint p2])
