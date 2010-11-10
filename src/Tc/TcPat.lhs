This module does the type checking / inference of patterns

> module Tc.TcPat where

> import Prelude.PreludeBuiltIn

> import Syntax.CoreSyn

> import Tc.Assumption 
> import Tc.TcMonad 
> import Tc.TcLit
> import Tc.TcUnify



> tcPat :: Pat Name -> TcM ([Pred Name], [Assumption Name], Ty Name)
> tcPat (PVar n) = do
>                     v <- newFreshTyVar Star
>                     return ([], [n :>: toScheme v], v)
> tcPat PWildCard = do
>                     v <- newFreshTyVar Star
>                     return ([], [], v)
> tcPat (PAs n pat) = do
>                       (ps, as, t) <- tcPat pat
>                       return (ps, (n :>: toScheme t):as, t)
> tcPat (PLit l) = do
>                     t <- tcLit l
>                     return ([], [], t)
> tcPat (PApp n pats) = do
>                         (ps, as, ts) <- tcPats pats
>                         t' <- newFreshTyVar Star
>                         sc <- lookupVarName n
>                         (qs :=> t) <- freshInst sc
>                         unify t (foldr TyFun t' ts)
>                         return (ps ++ qs, as, t')
> tcPat (PIr pat) = tcPat pat
> tcPat (PList pats) = do 
>                         (ps, as, ts) <- tcPats pats
>                         t' <- newFreshTyVar Star
>                         mapM_ (unify t') ts
>                         return (ps, as, tList t')
> tcPat (PTuple pats) = do
>                         (ps, as, ts) <- tcPats pats
>                         return (ps, as, tTuple ts)


> tcPats :: [Pat Name] -> TcM ([Pred Name], [Assumption Name], [Ty Name])
> tcPats pats = do
>                  psasts <- mapM tcPat pats
>                  let ps = concat [ps' | (ps', _, _) <- psasts]
>                      as = concat [as' | (_, as', _) <- psasts]
>                      ts =        [t | (_, _, t) <- psasts]
>                  return (ps, as, ts)
