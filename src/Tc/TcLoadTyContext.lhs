> {-#LANGUAGE FlexibleContexts#-}

This module aims to create the initial type context for the 
type checking / inference of a module. At the present it only build
a type context with the information provided in the module being 
compiled. In the future, this module will load interface files.

> module Tc.TcLoadTyContext where

> import Prelude.PreludeBuiltIn

> import Syntax.CoreSyn

> import Tc.Utils.Nameable
> import Tc.Class
> import Tc.Assumption
> import Tc.TcMonad hiding (emptyEnv)
> import Tc.TcSubst

> import Utils.Env
> import Utils.Stack

> mkContext :: [Class Name] -> [Assumption Name] -> TcEnv
> mkContext cs as = TcEnv {
>                       classenv = foldr insertN empty cs,
>                       varenv   = Stack [foldr insertN empty (as ++ builtincons)],
>                       subst    = nullSubst,
>                       fresh    = 0
>                   }

> insertN :: (HasName a Name, Ord (a Name)) => a Name -> Env (a Name) -> Env (a Name)
> insertN c e = insert (getName TypeAnalysis c) c e

> false = Unqual "False" :>: (Forall ([] :=> tBool))
> true = Unqual "True" :>: (Forall ([] :=> tBool))

