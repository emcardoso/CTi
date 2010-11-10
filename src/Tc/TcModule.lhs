> {-# LANGUAGE FlexibleContexts #-}

This module is the main driver of the type checking and inference
engine.

> module Tc.TcModule where

> import Control.Monad.Trans
> import Control.Monad

> import Data.List(nub)

> import Syntax.CoreSyn

> import Tc.Assumption
> import Tc.Utils.Nameable
> import Tc.Utils.DependAnalysis
> import Tc.TcUnify
> import Tc.TcMonad
> import Tc.TcWellformed
> import Tc.Class
> import Tc.TcSignatures
> import Tc.TcBinds

The main type checking driver

> tc :: TcEnv -> OverloadBinds -> Binds Name -> IO (Either String [Assumption Name])
> tc e os bs = do
>                 (r,e') <- runTcM (tcDriver os bs) e
>                 case r of
>                       Left err  -> return (Left err)
>                       Right as  -> return (Right $ nub as)

> tcDriver :: OverloadBinds -> Binds Name -> TcM [Assumption Name]
> tcDriver os bs = do
>                     os' <- addOverloadSignatures os
>                     pushSignatures bs
>                     tcModule (nub $ group (bs ++ os'))


> tcModule :: [Binds Name] -> TcM [Assumption Name]
> tcModule bss = do
>                   checkWellFormedInsts
>                   liftM concat $ mapM tcBinds bss

> checkWellFormedInsts :: TcM ()
> checkWellFormedInsts = do
>                           is <- getAllInsts
>                           mapM_ wf is


> pushSignatures :: Binds Name -> TcM ()                
> pushSignatures bs = let
>                       es = filter isExpl bs
>                       as = assumptions es
>                     in pushContext as
 

The first thing to be done is to build the dependencies groups

> group :: (HasName a Name, Show (a Name)) => [a Name] -> [[a Name]] 
> group ds = getDependentGroups ds (getName TypeAnalysis) (flip getReferencedNames (map (getName TypeAnalysis) ds))
