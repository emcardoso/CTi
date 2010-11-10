This file contains the basic definitions used by the desugar.

> module Desugar.Desugar(module Desugar.Desugar, throwError) where

We'll use a type family to model the result of desugaring a
expression.

> import Control.Monad.Error
> import Control.Monad.State

> import Haskell.Syntax.Syntax

> import Syntax.NameSyn

Defining a monad for the desugaring process and its operations

> type TypeSynEnv = [(HsType, HsType)]

> type DesugarM a = ErrorT String (StateT DesugarEnv IO) a

> data DesugarEnv = DesugarEnv {
>                      fresh      :: Int,
>                      typeSynEnv :: TypeSynEnv
>                   }

> emptyEnv = DesugarEnv {fresh = 0, typeSynEnv = []}

> newVar :: DesugarM Name
> newVar = do
>           i <- gets fresh
>           modify incFresh
>           return (Unqual ("_x" ++ show i))

> runDesugarM :: DesugarM a -> IO (Either String a, DesugarEnv)
> runDesugarM m = runStateT (runErrorT m) emptyEnv

Update functions for the environment

> incFresh :: DesugarEnv -> DesugarEnv
> incFresh d = DesugarEnv {fresh = 1 + (fresh d)}

> modifyTypeSyn :: TypeSynEnv -> DesugarEnv -> DesugarEnv
> modifyTypeSyn t d = DesugarEnv { typeSynEnv = t }

> modifyTypeSynEnv :: TypeSynEnv -> DesugarM ()
> modifyTypeSynEnv t = modify (modifyTypeSyn t)

> lookupTySyn :: HsType -> DesugarM (Maybe HsType)
> lookupTySyn ty = do 
>                     t <- gets (lookup ty . typeSynEnv) 
>                     return t

