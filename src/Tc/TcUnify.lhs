> {-#LANGUAGE FlexibleInstances, FlexibleContexts, TypeSynonymInstances #-}

This module defines the main engine of unification

> module Tc.TcUnify where

> import Control.Monad
> import Control.Monad.Error
> import Control.Monad.State

> import Syntax.CoreSyn

> import Tc.TcMonad
> import Tc.TcSubst
> import Tc.Assumption

> import Tc.Utils.Nameable
> import Data.Maybe

> import Utils.Pretty

a type class for calculate the most general unifier

> class Substitutable t => MGU t where
>     mgu :: t -> t -> TcM (TySubst Name) 


> instance (Pretty t, MGU t) => MGU [t] where
>     mgu (t:ts) (t':ts') = do
>                               s  <- mgu t t'
>                               s' <- mgu (apply s ts) (apply s ts')
>                               return (s' @@ s)
>     mgu [] []           = return nullSubst
>     mgu t t'            = throwError (concat ["Cannot unify constraints of different arities or diferent classes\n",
>                                                 unwords $ map (show . pprint) t, "\n", 
>                                                 unwords $ map (show . pprint) t'])


a type class for matching things

> class Substitutable t => Matchable t where
>     match :: t -> t -> TcM (TySubst Name)

> instance (Matchable t, Pretty t) => Matchable [t] where
>     match (t:ts) (t':ts') = do
>                               s  <- match t t'
>                               s' <- match (apply s ts) (apply s ts')
>                               merge s' s
>     match [] []           = return nullSubst
>     match t t'            = throwError (concat ["Cannot unify constraints of different arities or diferent classes\n",
>                                                 unwords $ map (show . pprint) t, "\n", 
>                                                 unwords $ map (show . pprint) t'])


instances for types 

> instance MGU (Ty Name) where
>     mgu (TyApp l r) (TyApp l' r') = do
>                                       s  <- mgu l l'
>                                       s' <- mgu (apply s r) (apply s r')
>                                       return (s' @@ s)
>     mgu (TVar v) t = varBind v t
>     mgu t (TVar v) = varBind v t
>     mgu (TCon tc) (TCon tc') | tc == tc' = return nullSubst
>                              | otherwise = throwError (concat ["Cannot unify\n", 
>                                                                 show $ pprint tc, 
>                                                                "\nwith:\n", 
>                                                                 show $ pprint tc'])
>     mgu (TyFun l r) (TyFun l' r') = do
>                                       s  <- mgu l l'
>                                       s' <- mgu (apply s r) (apply s r')
>                                       return (s' @@ s)
>     mgu t1@(TyTuple ts) t2@(TyTuple ts') 
>           = do
>               let len1 = length ts
>                   len2 = length ts'
>                   mgu' oldsub (t,t') = do 
>                                           newsub <- mgu (apply oldsub t) (apply oldsub t') 
>                                           return (newsub @@ oldsub)                                               
>               case len1 == len2 of
>                   True  -> foldM mgu' nullSubst (zip ts ts')
>                   False -> throwError (concat ["Cannot unify\n", 
>                                                 show $ pprint t1, 
>                                                 "\nwith:\n", 
>                                                 show $ pprint t2])
>     mgu t1@(TyList t) t2@(TyList t') = mgu t t'   
>     mgu t1 t2 = throwError (concat ["Cannot unify\n", show $ pprint t1, "\nwith:\n", show $ pprint t2])


> instance MGU (Pred Name) where
>     mgu (n,ts) (n',ts')
>               | n == n'   = mgu ts ts'
>               | otherwise = throwError (concat ["Cannot unify contraints of different classes:\n",
>                                                 show $ pprint n, "\n", show $ pprint n'])

> instance Matchable (Ty Name) where
>     match (TyApp l r) (TyApp l' r') = do
>                                          s  <- match l l'
>                                          s' <- match (apply s r) (apply s r')
>                                          merge s' s
>
>     match t1@(TVar (Skol i k)) t2@(TVar (Skol i' k')) = if (i == i')  && (k == k') 
>                                                          then return nullSubst 
>                                                          else throwError (concat ["Cannot match skolem constants \n", 
>                                                                               show $ pprint t1, 
>                                                                               "\nwith:\n", 
>                                                                               show $ pprint t2])
>
>     match (TVar v) t = return [(v,t)]
>     match (TCon tc) (TCon tc') | tc == tc' = return nullSubst
>                                | otherwise = throwError (concat ["Cannot match\n", 
>                                                                 show $ pprint tc, 
>                                                                "\nwith:\n", 
>                                                                 show $ pprint tc'])
>     match (TyFun l r) (TyFun l' r') = do
>                                       s  <- match l l'
>                                       s' <- match (apply s r) (apply s r')
>                                       merge s' s
>     match t1@(TyTuple ts) t2@(TyTuple ts') 
>           = do
>               let len1 = length ts
>                   len2 = length ts'
>                   match' oldsub (t,t') = do 
>                                           newsub <- match (apply oldsub t) (apply oldsub t') 
>                                           return (newsub @@ oldsub)                                               
>               case len1 == len2 of
>                   True  -> foldM match' nullSubst (zip ts ts')
>                   False -> throwError (concat ["Cannot match\n", 
>                                                 show $ pprint t1, 
>                                                 "\nwith:\n", 
>                                                 show $ pprint t2])

begin{ALT}

>     match t1@(TyAnd ts) t2@(TyAnd ts') 
>           = do
>               let len1 = length ts
>                   len2 = length ts'
>                   match' (t,t') = catchError (do 
>                                                  sub <- match  t t' 
>                                                  return (Just sub) )
>                                              (\e -> return Nothing)
>                  
>                   matchAny oldsub []     = return oldsub                                              
>                   matchAny oldsub (l:ls) = do l' <- mapM (\(x,y) -> match' (x , apply oldsub y) ) l
>                                               case [fromJust ms | ms <- l', isJust ms] of
>                                                   [s] -> matchAny (s @@ oldsub) ls
>                                                   
>                                                   _   -> throwError (concat ["Cannot match\n",
>                                                                             show $ pprint t1,
>                                                                             "\nwith:\n",
>                                                                             show $ pprint t2])
>                                                                                                                                     
>               case len1 == len2 of
>                   True  -> matchAny nullSubst $ map (\tau -> [(tau, tau') | tau' <- ts']) ts
                              
>                   False -> throwError (concat ["Cannot match\n", 
>                                                 show $ pprint t1, 
>                                                 "\nwith:\n", 
>                                                 show $ pprint t2])
>          
>

end{ALT}

>     match t1@(TyList t) t2@(TyList t') = match t t'
>     match t1 t2 = throwError (concat ["Cannot match\n", show $ pprint t1, "\nwith:\n", show $ pprint t2])



Binding a variable to a type

> varBind :: TyVar Name -> Ty Name -> TcM (TySubst Name)
> varBind v t
>       | t == TVar v   = return nullSubst
>       | v `elem` tv t = throwError (concat ["Occurs check fails in types:\n", 
>                                              show $ pprint v, "\nand\n", show $ pprint t])
>       | (kind v) /= (kind t) = throwError (concat ["The kinds of:\n", 
>                                                     show $ pprint v,
>                                                     "\nand\n",
>                                                     show $ pprint t,
>                                                     "\ndoesnt match"])
>       | otherwise =  return [(v, t)]


Unification -> extends current substitution and returns the mgu

> unify1 :: (MGU t, Substitutable t) => Bool -> t -> t -> TcM (TySubst Name)
> unify1 ext t1 t2 = do
>                        s <- mgu t1 t2
>                        when ext (extendSubst s)
>                        return s

> unify :: (MGU t, Substitutable t) => t -> t -> TcM (TySubst Name)
> unify t1 t2 = unify1 True t1 t2

This functions is used by the type ordering

> unifyOrd :: (MGU t, Substitutable t) => t -> t -> TcM (TySubst Name)
> unifyOrd t1 t2 = unify1 False t1 t2

Begin{ALTER}


end{ALTER}
