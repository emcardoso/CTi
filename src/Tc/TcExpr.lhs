This module does the type checking / inference for expressions

> module Tc.TcExpr where

> import Control.Monad
> import Control.Monad.Trans

> import Prelude.PreludeBuiltIn(tBool)

> import Syntax.CoreSyn

> import Tc.Assumption 
> import Tc.TcMonad 
> import Tc.TcLit
> import Tc.TcUnify
> import Tc.TcSubst
> import Tc.TcPat
> import Tc.TcBinds

> import Utils.Pretty


begin{ALTER:}

> check :: Expr Name -> Ty Name  -> TcM ([Pred Name], Ty Name)
> check v@(Var _) te = do 
>                         (ps,tv) <- tcExpr v
>                         unify tv te
>                         s <- getSubst
>                         return (apply s ps, apply s tv)
>                        
> check (App l r) te = do 
>                         (ps, tl) <- tcExpr l
>                         (t_arg,t_res) <- splitFun tl
>                         (qs,tr)  <- check r t_arg
>                         s <- getSubst
>                         return (apply s (ps ++ qs),te)                                                    
> 
> check e te = do
>                 (ps,ty) <- tcExpr e
>                 unify ty te
>                 s <- getSubst
>                 return (apply s ps, apply s ty)
>                  
>

end{ALTER}

checking expressions

> tcExpr :: Expr Name -> TcM ([Pred Name], Ty Name)
> tcExpr (Var v) = do
>                     sc         <- lookupVarName v
>                     (ps :=> t) <- freshInst sc
>                     return (ps, t)
> tcExpr (Con c) = do
>                     sc         <- lookupVarName c
>                     (ps :=> t) <- freshInst sc
>                     return (ps,t)
> tcExpr (Const l) = liftM ((,) []) (tcLit l)
> tcExpr (Lam pats e) = do
>                          (ps, as, ts) <- tcPats pats
>                          (ps',t)      <- context as (tcExpr e)
>                          return (ps ++ ps', foldr TyFun t ts)

> tcExpr e@(App l r) = do
>                       t1        <- newFreshTyVar Star
>                       check e t1

>{- tcExpr (App l r) = do
>                       (ps, tl) <- tcExpr l
>                       (qs, tr) <- tcExpr r
>                       t        <- newFreshTyVar Star
>                       unify (TyFun tr t) tl
>                       s <- getSubst
>                       return (apply s (ps ++ qs), apply s t) -}


> tcExpr (Case e alts) = do
>                           (ps,te) <- tcExpr e
>                           (qs,t)  <- tcAlts alts
>                           return (ps ++ qs, t)
> tcExpr (If e1 e2 e3) = do
>                           (ps1, t1) <- tcExpr e1
>                           (ps2, t2) <- tcExpr e2
>                           (ps3, t3) <- tcExpr e3
>                           unify tBool t1
>                           unify t2 t3
>                           s <- getSubst
>                           return (apply s $ ps1 ++ ps2 ++ ps3, apply s t2)
> tcExpr (List es) = do
>                       psts <- mapM tcExpr es
>                       let
>                           ps' = concatMap fst psts
>                           ts' = map snd psts
>                       t <- newFreshTyVar Star
>                       mapM (unify t) ts'
>                       return (ps', TyList t)
> tcExpr (Tuple es) = do
>                       psts <- mapM tcExpr es
>                       let
>                           ps' = concatMap fst psts
>                           ts' = map snd psts
>                       return (ps', TyTuple ts')
> tcExpr (Let bs e) = do
>                       as     <- tcBinds bs
>                       (ps,t) <- context as (tcExpr e)
>                       return (ps, t)

checking a set of alternatives

> tcAlts :: Alts Name -> TcM ([Pred Name], Ty Name)
> tcAlts alts = do
>                   psts <- mapM tcAlt alts
>                   let
>                       ps'     = concatMap fst psts
>                       (t:ts)  = map snd psts
>                   mapM_ (unify t) ts
>                   s <- getSubst
>                   return (apply s ps', apply s t)

checking one alternative

> tcAlt :: Alt Name -> TcM ([Pred Name], Ty Name)
> tcAlt (pats, e) = do
>                      (ps, as, ts) <- tcPats pats
>                      (qs, t)      <- context as (tcExpr e)
>                      return (ps ++ qs, foldr TyFun t ts)

 -- begin{ALTER}

> splitFun :: Ty Name -> TcM(Ty Name, Ty Name)
> splitFun tFun = do 
>                    arg    <- newFreshTyVar Star 
>                    result <- newFreshTyVar Star
>                    unify tFun (TyFun arg result) 
>                    s <- getSubst
>                    return (apply s arg, apply s result)

 -- end{alter}