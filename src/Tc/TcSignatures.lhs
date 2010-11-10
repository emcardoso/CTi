This module contains functions for tasks related with type
signatures in binds.

> module Tc.TcSignatures where

> import Control.Monad
> import Control.Monad.Error
> import Data.Generics

> import Syntax.CoreSyn

> import Tc.Utils.Nameable
> import Tc.Assumption
> import Tc.TcMonad
> import Tc.Class

> import Utils.Pretty

This function adds types signatures for overloaded symbols defined elsewhere.

> addOverloadSignatures :: OverloadBinds -> TcM (Binds Name)
> addOverloadSignatures os = do
>                               bs  <- liftM concat $ mapM (uncurry addClassBinds) (classBinds os)
>                               bs' <- liftM concat $ mapM (uncurry addInstBinds) (instsBinds os)
>                               return (bs ++ bs')

> addClassBinds :: Class Name -> Binds Name -> TcM (Binds Name)
> addClassBinds c bs = mapM (addBind c) bs
>                      where
>                         addBind c f@(PatBind p _ e) 
>                               = let
>                                   sigs = members c
>                                 in case findName (getName undefined f) sigs of
>                                           Just (_ :>: s) -> return $ PatBind p (Just s) e
>                                           Nothing -> throwError (concat ["The symbol:\n",
>                                                                           show $ pprint p,
>                                                                          "\nisnt visible in class:\n",
>                                                                           show $ pprint (name c)]) 
>                         addBind c (FunBind n _ as) 
>                               = let
>                                   sigs = members c
>                                 in case findName n sigs of
>                                           Just (_ :>: s)  -> return $ FunBind n (Just s) as
>                                           Nothing -> throwError (concat ["The symbol:\n",
>                                                                           show $ pprint n,
>                                                                          "\nisnt visible in class:\n",
>                                                                           show $ pprint (name c)]) 

> addInstBinds :: Inst Name -> Binds Name -> TcM (Binds Name)
> addInstBinds i bs = mapM (addInst i) bs

> addInst :: Inst Name -> Bind Name -> TcM (Bind Name)
> addInst i n@(PatBind p _ e) = do
>                               c <- getClass (instname i)
>                               let sigs = members c
>                                   vs   = parameters c
>                               case findName (getName undefined n) sigs of
>                                    Just (_ :>: s) 
>                                         -> return (PatBind p (fixInstTy i vs s) e)
>                                    Nothing -> throwError (concat ["The symbol:\n",
>                                                                   show $ pprint p,
>                                                                  "\nisnt visible in class:\n",
>                                                                   show $ pprint (name c)]) 
> addInst i (FunBind n _ as) = do
>                               c <- getClass (instname i)
>                               let sigs = members c
>                                   vs   = parameters c
>                               case findName n sigs of
>                                    Just (_ :>: s) 
>                                         -> return (FunBind n (fixInstTy i vs s) as)
>                                    Nothing -> throwError (concat ["The symbol:\n",
>                                                                   show $ pprint n,
>                                                                  "\nisnt visible in class:\n",
>                                                                   show $ pprint (name c)]) 


> fixInstTy :: Inst Name -> [TyVar Name] -> Scheme Name -> Maybe (Scheme Name)
> fixInstTy i vs s@(Forall qt) = let
>                                   ts = instparameters i
>                                   s  = zip vs ts
>                                   subst s (TVar v) = case lookup v s of
>                                                           Just ty -> ty
>                                                           Nothing -> TVar v
>                                   subst s t = t
>                                in Just (Forall $ everywhere (mkT (subst s)) qt)                            

This function adds a kind-checked type signature in a bind

> addNewSigs :: [Assumption Name] -> Binds Name -> Binds Name
> addNewSigs sigs bs = map addNewSig bs
>                       where
>                          addNewSig f@(FunBind n _ as) 
>                               = case findName n sigs of
>                                       Just (_ :>: s) ->  FunBind n (Just s) as
>                                       Nothing        ->  f     
>                          addNewSig f@(PatBind p _ e) 
>                               = case findName (getName undefined f) sigs of
>                                       Just (_ :>: s) -> PatBind p (Just s) e
>                                       Nothing        -> f


> findName _ [] = Nothing
> findName n (s@(n' :>: _):ss) 
>             | n == n' = Just s
>             | otherwise = findName n ss
