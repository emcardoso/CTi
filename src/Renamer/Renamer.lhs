This module contains the basic definitions for
the renaming algorithm

> module Renamer.Renamer where

> import Control.Monad.Error
> import Control.Monad.State
> import Data.Maybe (fromJust)

> import Haskell.Syntax.Syntax
> import Haskell.Pretty.Pretty

> import qualified Data.Map as Map

> import Utils.Stack


The renaming environment

> data RenamerEnv = RenamerEnv {
>                     fresh :: Int,                       -- a supply for fresh names
>                     env   :: Stack (Map.Map HsName Int) -- like a stack, for representing scopes
>                  } deriving (Eq, Ord, Show)

> emptyEnv :: RenamerEnv
> emptyEnv = RenamerEnv { fresh = 0, env = Stack [Map.empty] }


The renaming monad

> type RenamerM a = ErrorT String (StateT RenamerEnv IO) a


> freshM :: RenamerM Int
> freshM = do 
>             renv <- get 
>             let n = fresh renv
>             put (renv{fresh  = 1 + (fresh renv)})
>             return n

> pushScope :: RenamerM ()
> pushScope = do
>               renv <- get
>               let e = env renv
>               put (renv{ env = push Map.empty e , fresh = (1 + fresh renv) })

> popScope :: RenamerM ()
> popScope = do
>               renv <- get
>               let e = env renv
>               case pop e of
>                   Nothing    -> throwError "Trying to pop from empty scope"
>                   Just (x,s) -> put (renv{ env = s , fresh = fresh renv})

> blockScope :: RenamerM a -> RenamerM a
> blockScope m = do
>                   pushScope
>                   x <- m
>                   popScope
>                   return x

> putInEnv :: [HsName] -> RenamerM [HsName]
> putInEnv ns = do
>                 e <- get
>                 venv <- gets env
>                 vs <- mapM (const freshM) ns
>                 let venv' = venv `unionM` (Map.fromList $ zip ns vs)
>                     ns'   = zipWith renameN ns vs
>                     renameN (HsIdent n) v = HsIdent (n ++ "_" ++ show v)
>                     renameN (HsSymbol n) v = HsSymbol(n ++ "_" ++ show v)
>                     unionM p m = case pop p of 
>                                       Nothing      -> push m p
>                                       Just (x, p') -> push (m `Map.union` x) p'
>                 put (e{ env = venv'})
>                 return ns'

False -> Create a new var instead of throw a error

> lookupEnv :: Bool -> HsName -> RenamerM Int
> lookupEnv varExist n = do
>                           e <- gets env
>                           case peek e of
>                                Nothing -> throwError ("Cannot find a valid scope!")
>                                Just m  -> case Map.lookup n m of
>                                                  Nothing -> if varExist then 
>                                                                 throwError ("Cannot find the following type variable\n" 
>                                                                               ++ (prettyPrint n))
>                                                               else do 
>                                                                       putInEnv [n]
>                                                                       lookupEnv varExist n
>                                                  Just i  -> return i

running the monad

> runRenamerM :: RenamerM a -> IO (Either String a, RenamerEnv)
> runRenamerM m = runStateT (runErrorT m) emptyEnv
