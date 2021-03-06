This module does the type checking / inference of bindings

> module Tc.TcBinds where

> import Control.Monad
> import Control.Monad.Trans
> import Control.Monad.Error

> import Data.List
> import Data.Maybe

> import Syntax.CoreSyn

> import Tc.Assumption 
> import Tc.TcMonad 
> import Tc.TcLit
> import Tc.TcUnify
> import Tc.TcPat
> import Tc.TcSubst
> import Tc.Utils.Nameable
> import {-# SOURCE #-} Tc.TcExpr
> import Tc.TcWellformed

> import Utils.Pretty

> tcBinds :: Binds Name -> TcM [Assumption Name]
> tcBinds bs = let (es,is) = partition isExpl bs
>                  as = assumptions es
>                in do 
>                     (ps, as') <- tcImpls is
>                     pss <- context as' (mapM tcExpl es)
>                     return (as ++ as')

begin{ALTER}

tcExpl antes da modificação feita para
incorporar Intersection Types

 tcExpl :: Bind Name ->  TcM [Pred Name]
 tcExpl (FunBind n (Just sc) (Just alts)) 
           = do
                (ps :=> t) <- freshInst sc
                (ps', t') <- tcAlts alts
                s' <- match t' t
                s <- wf (apply s' $ ps' :=> t')
                ps'' <- improve ps'
                pc   <- improve ps
                sc'' <- quantify (apply (s @@ s') (ps'' :=> t'))
                sc'  <- quantify (pc :=> apply (s @@ s') t)
                if sc' /= sc'' then 
                       throwError ("Type Signature error in\n" ++ (show $ pUnlines [(n :>: sc), (n :>: sc')]))
                   else return (apply s ps')                    

end{ALTER}

begin{ALTER}

> tcExpl :: Bind Name ->  TcM [Pred Name]
> tcExpl (FunBind n (Just sc) (Just alts)) 
>           = do
>                (ps :=> t) <- freshInst sc
>                 
>                (ps', t') <- tcAlts' (skol t) alts
>                s' <- match t' t
>                s <- wf (apply s' $ ps' :=> t')
>                ps'' <- improve ( apply (s @@ s') ps')
>                pc   <- improve ( apply (s @@ s') ps )
>                sc'' <- quantify (apply (s @@ s') (ps'' :=> t'))
>                sc'  <- quantify (pc :=> apply (s @@ s') t)
>                if sc' /= sc'' then 
>                       throwError ("Type Signature error in\n" ++ (show $ pUnlines [(n :>: sc), (n :>: sc')]))
>                   else return (apply (s @@ s') ps')                    

end{ALTER}

> tcExpl f@(PatBind p (Just sc) e)
>           = do
>               (ps :=> t)  <- freshInst sc
>               (qs,as, ty) <- tcPat p
>               (ps', t') <- context as (tcExpr e)
>               s' <- match t' t
>               s <- wf (apply s' $ ((ps' ++ qs) :=> t'))
>               qs' <- improve (apply (s @@ s') (ps' ++ qs))
>               pc  <- improve ps
>               sc' <- quantify (pc :=> t)
>               sc'' <- quantify (apply (s @@ s') (qs' :=> t'))
>               let n = getName undefined f
>               if sc' /= sc'' then
>                   throwError ("Type Signature error in\n" ++ (show $ pUnlines [(n :>: sc''), (n :>: sc')]))
>                 else return qs'

> tcImpls :: Binds Name -> TcM ([Pred Name], [Assumption Name])
> tcImpls bs = do
>                ts <- mapM (\_ -> newFreshTyVar Star) bs
>                let 
>                   ns = map (getName TypeAnalysis) bs
>                   scs = map toScheme ts
>                   as  = zipWith (:>:) ns scs
>                pssts <- context as (mapM tcImpl bs)
>                let
>                  pss' = map fst pssts
>                  as' = map snd pssts
>                  ts' = map tyFrom as'
>                  tyFrom (_ :>: (Forall (_ :=> t))) = t 
>                zipWithM_ unify ts ts'
>                s <- getSubst
>                as'' <- liftM (zipWith (:>:) ns) (mapM quantify (zipWith (:=>) pss' ts'))  
>                return (apply s $ concat pss', as'')                   


> tcImpl :: Bind Name -> TcM ([Pred Name], Assumption Name)
> tcImpl (FunBind n Nothing (Just alts)) = do
>                                             (ps, t) <- tcAlts alts
>                                             s <- wf (ps :=> t)
>                                             return (apply s ps, n :>: (Forall (apply s (ps :=> t))))
> tcImpl k@(PatBind p Nothing e) = do
>                                   (ps, as, t) <- tcPat p
>                                   (qs,t') <- context as (tcExpr e)
>                                   s' <- wf ((ps ++ qs) :=> t')
>                                   return (apply s' $ ps ++ qs, (getName undefined k) :>: (Forall $ apply s' ((ps ++ qs) :=> t')))


> assumptions = catMaybes . map f
>               where
>                   f (FunBind n (Just s) _) = Just (n :>: s)
>                   f (FunBind _ _        _) = Nothing
>                   f p@(PatBind _ (Just s) _) = Just (getName undefined p :>: s)
>                   f (PatBind _ _ _)          = Nothing

> isExpl (FunBind _ (Just _) _) = True
> isExpl (PatBind _ (Just _) _) = True
> isExpl _                      = False


> improve :: [Pred Name] -> TcM [Pred Name]
> improve ps = do
>                   s <- getSubst
>                   let ps' = apply s ps
>                   return [delta | delta <- ps', not $ null $ tv delta]


> getIntersecVars :: Ty Name -> [Ty Name] 
> getIntersecVars (TyFun arg res) = getIntersecVars arg `union` getIntersecVars res
> getIntersecVars (TyList t)   = getIntersecVars t
> getIntersecVars (TyTuple lt) = foldr (\x lt' -> union (getIntersecVars x) lt') [] lt
> getIntersecVars (TyAnd lt)   = foldr (\x lt' -> union (getVars x) lt') [] lt  
> getIntersecVars (TVar _) = []
> getIntersecVars (TCon _) = []

> getVars :: Ty Name -> [Ty Name] 
> getVars (TyFun arg res) = getVars arg `union` getVars res
> getVars (TyList t) = getVars t
> getVars (TyTuple lt) = foldr (\x lt' -> union (getVars x) lt') [] lt
> getVars (TyAnd lt) = foldr (\x lt' -> union (getVars x) lt') [] lt  
> getVars x@(TVar _) = [x]
> getVars (TCon _) = []

> skol :: Ty Name -> Ty Name
> skol t = let ivars = getIntersecVars t
>                   in skol' ivars t   
>   where
>     skol' l (TyFun arg res) =  TyFun (skol' l  arg)  (skol' l res)
>     skol' l (TyList t)   =  TyList ( skol' l t)
>     skol' l (TyTuple lt) =  TyTuple (map (skol' l) lt)
>     skol' l (TyAnd lt)   =  TyAnd (map (skol' l) lt)  
>     skol' l x@(TVar (Free i k)) = if (elem x l) then (TVar (Skol i k)) else x
>     skol' l x = x 