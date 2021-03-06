This module does the type checking / inference for expressions

> module Tc.TcExpr where

> import Control.Monad
> import Control.Monad.Trans
> import Control.Monad.Error(throwError, catchError) -- ALTER

> import Debug.Trace

> import Data.Maybe(isJust)

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

> import Data.List(union)



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

begin{ALTER}
 Application rule before modification
 tcExpr (App l r) = do
                       (ps, tl) <- tcExpr l
                       (qs, tr) <- tcExpr r
                       t        <- newFreshTyVar Star
                       unify (TyFun tr t) tl
                       s <- getSubst
                       return (apply s (ps ++ qs), apply s t) 
end{ALTER}

begin{ALTER}

 tcExpr (App l@(Var _) r) = do
                               (qs, tr) <- tcExpr r
                               (ps, tl) <- tcExpr l
                               insti (apply s tl) tr
                               s'' <- getSubst
                               return ((apply s'' ps) ++ qs, tr') 

>
>
> tcExpr (App l r) = do
>                               (ps, tl) <- tcExpr l
>                               if isFun tl                               
>                                  then do
>                                          (ty_param,ty_res) <-  splitFun tl
>                                          (qs, tr) <- tcExpr r 
>                                          (qs',tr') <- geni (ty_param) tr qs
>                                          s' <- getSubst 
>                                          return (apply s' (ps ++ qs'), apply s' ty_res) 
>
>                                  else do
>                                          (qs, tr) <- tcExpr r
>                                          tr' <- insti tl tr
>                                          s'' <- getSubst
>                                          return ((apply s'' ps) ++ qs, apply s'' tr')                                      
>

end{ALTER}

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


begin{ALT}

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

>
> tcAlts' :: Ty Name -> Alts Name -> TcM ([Pred Name], Ty Name)
> tcAlts'  t alts = do
>                        psts <- mapM (tcAlt' t ) alts
>                        let
>                            ps'     = concatMap fst psts
>                            (t:ts)  = map snd psts
>                        mapM_ (unify t) ts
>                        s <- getSubst
>                        return (apply s ps', apply s t)


> tcAlt' :: Ty Name -> Alt Name -> TcM ([Pred Name], Ty Name)
> tcAlt' t (pats, e) = do
>                       (ps, as, ts) <- tcPats' (args pats t)
>                       (qs, t')      <- context as (tcExpr e)
>                       return (ps ++ qs, foldr TyFun t' ts)
>    where
>      args [] t = [] 
>      args (pat:pats) (TyFun arg res) = (pat,arg) : args pats res
>      args (pat:pats) _               = error " Invalid type annotation."

end{ALT}

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

> 
> isVar :: Expr Name -> Bool
> isVar (Var _) = True
> isVar _       = False

 -- end{alter}

begin{ALTER}

Intersection type generalization rule

>
> geni :: Ty Name -> Ty Name -> [Pred Name] -> TcM ([Pred Name],Ty Name)
> geni (TyAnd ts) tau ps = do 
>                             sc <- quantify (ps :=> tau)
>                             geni2' ts sc ([],nullSubst) 
> geni tau tau' ps = do
>                       s' <- getSubst
>                       s <-  unify (apply s' tau) (apply s' tau')
>                       return (apply s ps, apply s tau')
>
>
> geni2' :: [Ty Name] -> Scheme Name -> ([Pred Name],TySubst Name) ->  TcM ([Pred Name], Ty Name)
> geni2' [] sc (preds,sub) =  do  (qs :=> t') <- freshInst sc  
>                                 extendSubst sub
>                                 return (apply sub  preds, t')  
> geni2' (tau:taus) sc (preds,sub) = do 
>                                      (qs :=> t') <- freshInst sc  
>                                      s <- trace ( "*** {geni2} t'= " ++ show (pprint t') ++ "   tau = " ++ show (pprint tau) ) $ unify t' tau
>                                      let s' = s @@ sub
>                                      preds' <- return $ preds ++ (apply s'  qs)
>                                      sc' <- quantify (qs :=> t')
>                                      (qs' :=> t'')  <- trace ("s = " ++ show  s') (freshInst sc')
>                                      sc'' <- quantify (apply s' (qs' :=> t''))
>                                      geni2' (apply s' taus)  sc'' (trace ("\n***t'' =  " ++ show t'') (preds',s'))

Intersection type instantiation rule.
For use in apllication rule only

> insti :: Ty Name -> Ty Name -> TcM (Ty Name)
> insti (TyAnd ts) tau =  do 
>                            a  <- newFreshTyVar Star
>                            ls <- mapM (\tau' -> maybeMatch (TyFun tau a) tau') ts
>                            case (filter isJust ls) of
>                                [Just s] -> do 
>                                               extendSubst s
>                                               return (apply s a)
>                                _   ->  throwError ("Ambiguos instersection type")
>
> insti tau' tau = do   
>                    a <- newFreshTyVar Star
>                    unify (TyFun tau a) tau'
>                    s <- getSubst
>                    return (apply s a)
>
>
> maybeMatch t1 t2 = catchError (match t1 t2 >>= (\s -> return $ Just s)  ) (\_ -> return Nothing)
>
> isIntersection :: Ty Name -> Bool
> isIntersection (TyAnd _) = True
> isIntersection _        = False
>
> isFun :: Ty Name -> Bool
> isFun (TyFun _ _ ) = True
> isFun _          = False

end{ALTER}
