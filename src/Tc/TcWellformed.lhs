> {-#LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

This module contains the definition of a well-formed type. 
Additionally, this module contains the definition of well-formed 
instance constraints.

> module Tc.TcWellformed where

> import Control.Monad.Error

> import Data.List

> import Syntax.CoreSyn

> import Tc.TcMonad
> import Tc.TcSat
> import Tc.TcSubst
> import Tc.TcInst
> import Tc.TcUnify
> import Tc.Class

> import Utils.Pretty

A class definition for well formed property

> class Wf a where
>   wf :: a -> TcM (TySubst Name)

> instance Wf (Qual Name) where
>   wf t@(kappa :=> ty) = let
>                             kappa0 = kappa \\ (kappa |*: (tv ty))
>                           in sat kappa0

> instance Wf (Scheme Name) where
>   wf s = do
>             qt' <- freshInst s
>             wf qt'

> instance Wf (Inst Name) where
>   wf i@(Inst n ts kappa) = do
>                               c <- getClass n
>                               let vs = parameters c
>                               sc <- match (map TVar vs) ts  -- condition 2
>                               let
>                                  toPred (n,vs) = (n, map TVar vs)
>                                  kc = map toPred $ supers c
>                                  sckc = apply sc kc
>                                  kappa0 = [c | c <- sckc, null $ tv c]
>                                  condition1 = tv kappa `subseteq` tv ts
>                                  condition3 = (sckc \\ kappa0) `subseteq` kappa
>                               condition4 <- checkInsts kappa0
>                               if and [condition1, condition3, condition4] then
>                                   return sc
>                                 else throwError ("Invalid Instance:" ++ (show $ pprint i))

Checking if instances present in kappa0 are present in context.

> checkInsts :: [Pred Name] -> TcM Bool
> checkInsts = allM checkInst

> checkInst :: Pred Name -> TcM Bool
> checkInst p@(n, _) = do
>                         let toPred i = (instname i, instparameters i)
>                         is <- liftM (map toPred) (getInsts n)
>                         return (p `elem` is)

Constraint set closure implementation

> (|*:) :: [Pred Name] -> [TyVar Name] -> [Pred Name]
> ps |*: v = let kv = ps |: v in 
>               if tv kv `subseteq` v then kv
>                  else ps |*: (tv kv)


simple projection of a constraint set 

> (|:) :: [Pred Name] -> [TyVar Name] -> [Pred Name]
> ps |: v = [(x, ts) | (x,ts) <- ps, not $ null $ tv ps `intersect` v]
