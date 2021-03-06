This module contains the type checking / infernece monad and its
operations

> module Tc.TcMonad where

> import Control.Monad.Error
> import Control.Monad.State

> import Data.Char
> import Data.Map (showTree)
> import Data.Maybe(isJust)

> import Syntax.CoreSyn

> import Tc.Class
> import Tc.Assumption
> import Tc.TcSubst
> import Tc.TcInst

> import qualified Utils.Env as Env
> import Utils.Pretty
> import Utils.Stack

definition of the type checking environment.

> type ClassEnv = Env.Env (Class Name)

> type VarEnv = Env.Env (Assumption Name)

> data TcEnv = TcEnv {
>                 classenv :: ClassEnv,         -- environment for type class constraints
>                 varenv   :: Stack VarEnv,           -- variable / data constructors environment
>                 fresh    :: Int,              -- fresh variable supplier
>                 subst    :: TySubst Name      -- current substitution           
>              } deriving (Eq, Ord, Show)

> emptyEnv = TcEnv {
>               classenv = Env.empty,
>               varenv = Stack [Env.empty],
>               fresh = 0,
>               subst = nullSubst                
>            }

A monad for the type checking / inference process

> type TcM a = ErrorT String (StateT TcEnv IO) a

> runTcM :: TcM a -> TcEnv -> IO (Either String a, TcEnv)
> runTcM m e = runStateT (runErrorT m) e


Looking up values in the type context

> lookupVarName :: Name -> TcM (Scheme Name)
> lookupVarName n = do
>                     r <- gets (peek . varenv)
>                     case r of
>                        Just m  -> case Env.lookup n m of
>                                       Just (_ :>: s) -> return s
>                                       Nothing -> do
>                                                    tryToFindInOverloadedDefs n 
>                        Nothing -> error "Pop an empty type context"

> tryToFindInOverloadedDefs :: Name -> TcM (Scheme Name)
> tryToFindInOverloadedDefs n = do
>                                  ms <- gets (concatMap members . Env.elems . classenv)
>                                  case findByName n ms of
>                                       Just s  -> return s
>                                       Nothing -> throwError ("Cannot find a assumption for:\n" ++ (show $ pprint n))

> findByName n [] = Nothing
> findByName n (s@(n' :>: sc):ss) 
>               | n == n' = Just sc
>               | otherwise = findByName n ss

Operations over the monad

> context :: [Assumption Name] -> TcM a -> TcM a
> context as m = do
>                   pushContext as
>                   x <- m
>                   popContext
>                   return x

> pushContext :: [Assumption Name] -> TcM ()
> pushContext as = do 
>                     e  <- get
>                     let ve = varenv e
>                     case peek ve of
>                       Just m -> do
>                                   let m' = foldr ins m as
>                                       ins a@(n :>: _) m = Env.insert n a m
>                                   put(e{ varenv = push m' ve } )
>                       Nothing -> error "Pop an empty type context"

> popContext :: TcM ()
> popContext = do
>                e <- get
>                let ve = varenv e
>                case pop ve of
>                   Just (_, p) -> put ( e{ varenv = p } )
>                   Nothing     -> error "Pop an empty type context"


> getSubst :: TcM (TySubst Name)
> getSubst = gets subst


> extendSubst :: TySubst Name -> TcM ()
> extendSubst s = do
>                   e <- get
>                   put (e{ subst = s @@ (subst e)})


> freshM :: TcM Int
> freshM = do
>             e <- get
>             let i = fresh e
>             put (e{ fresh = 1 + (fresh e)})
>             return i


> newFreshTyVar :: Kind -> TcM (Ty Name)
> newFreshTyVar k = do
>                      n <- freshM  
>                      return (TVar $ Free n k)

> freshInst :: Scheme Name -> TcM (Qual Name)
> freshInst (Forall qt) = do
>                           let vars = bv qt
>                           ts <- mapM (newFreshTyVar . kind) vars
>                           return (instantiate vars ts qt)

> quantify :: (Qual Name) -> TcM (Scheme Name)
> quantify qt = let
>                  vs = fv qt
>                  s  = foldr step [] $ zip vs [0..]
>                  step (v,n) ac = (v, TVar $ Bound (toName n) (kind v)) : ac
>                  toName = Unqual . (:[]) . chr . (+ 97)
>               in return (Forall (apply s qt))

> getInsts :: Name -> TcM [Inst Name]
> getInsts c = do 
>                cenv <- gets (Env.lookup c . classenv)
>                case cenv of
>                   Nothing -> throwError ("Cannot find class:\n" ++ (show $ pprint c))
>                   Just c' -> return (instances c')


> getAllInsts :: TcM [Inst Name]
> getAllInsts = gets (concatMap instances . Env.elems . classenv)

> getClass :: Name -> TcM (Class Name)
> getClass n = do
>                cenv <- gets (Env.lookup n . classenv)
>                case cenv of
>                   Nothing -> throwError ("Cannot find class:\n" ++ (show $ pprint n))
>                   Just c' -> return c'


> toScheme :: Ty Name -> Scheme Name
> toScheme t = Forall ([] :=> t)
