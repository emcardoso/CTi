This module implements the constraint set satisfiability
algorithm.

> module Tc.TcSat where

> import Control.Monad
> import Control.Monad.Error

> import Data.List
> import qualified Data.Map as Map

> import Syntax.CoreSyn

> import Tc.Class 
> import Tc.TcMonad
> import Tc.TcSubst
> import Tc.TcOrdering
> import Tc.TcUnify

> import Utils.Pretty

> type ConstraintMap = Map.Map Name [Pred Name]

> type SatSubst = Map.Map (TyVar Name) [Ty Name]

The constraint set satisfiability algorithm 

> sat :: [Pred Name] -> TcM (TySubst Name)
> sat kappa = liftM toTySubst (sati kappa Map.empty Map.empty)

> sati :: [Pred Name] -> ConstraintMap -> SatSubst -> TcM SatSubst
> sati []  _  s    = do
>                       return s
> sati [k] m  s    = do
>                       liftM fst $ sat1 k (Map.insert (fst k) [] m) s
> sati (k:ks) m  s = do
>                      (s1,m1) <- sat1 k m s
>                      let s1' = toTySubst s1
>                          nonull = not . null . fv
>                          notSolved ks = [delta | delta <- ks, nonull ks]
>                      sati (notSolved $ apply s1' ks) m1 s1

> sat1 :: Pred Name -> ConstraintMap -> SatSubst -> TcM (SatSubst, ConstraintMap)
> sat1 p@(n,ts) m s = do
>                       insts <- getInsts n   
>                       unifysucceed <- someUnify p insts 
>                       typeordcond  <- liftM not (allLessThan p m)
>                       predvisited  <- wasVisited p m
>                       if unifysucceed && typeordcond then 
>                               throwError (unsatisfiability p)    
>                          else if unifysucceed && predvisited then
>                                    return (s, m)
>                                  else if unifysucceed then satrec p s m insts
>                                           else throwError (unsatisfiability p)    

> satrec :: Pred Name -> SatSubst -> ConstraintMap -> [Inst Name] -> TcM (SatSubst, ConstraintMap)
> satrec p _ _ [] = throwError (unsatisfiability p)
> satrec p s m insts = do
>                        (s1,m') <- foldM (step p) (s, m) insts
>                        s' <- substIntersect p s s1 
>                        return (s',m')

> substIntersect :: Pred Name -> SatSubst -> SatSubst -> TcM SatSubst
> substIntersect p s s1 = do
>                           cond <- condition 
>                           if cond then throwError (unsatisfiability p)
>                             else Map.foldWithKey (inter s) (return s1) s1
>                       where
>                           inter :: SatSubst -> TyVar Name -> [Ty Name] -> TcM SatSubst -> TcM SatSubst
>                           inter s' v ts s = case Map.lookup v s' of
>                                                   Nothing  -> s
>                                                   Just tis -> do
>                                                                 ts'' <- filterM (unify' ts) tis
>                                                                 su <- getSubst
>                                                                 s'' <- s
>                                                                 return (Map.union (Map.singleton v (apply su ts'')) s'')
>                           condition = Map.foldWithKey (check s') (return False) s1
>                           s' = s `Map.intersection` s1'
>                           s1' = Map.filter sing s1
>                           sing [x] = True
>                           sing  _  = False
>                           unify' ts t' = allM (unify'' t') ts 
>                           unify'' t t' = handle (unify t t') True False 
>                           check s' v ts b = case Map.lookup v s' of
>                                                   Just ts' -> allM (unify' ts) ts'
>                                                   Nothing  -> b

> step :: Pred Name -> (SatSubst, ConstraintMap) -> Inst Name -> TcM (SatSubst, ConstraintMap)
> step p (s,m) inst = do
>                       s' <- mgu p p' `catchError` (const (return nullSubst))
>                       let mx = Map.fromList $ map wrap s'
>                       liftM (\s -> (s,m')) (sati kappa m' (s +: mx))
>                     where
>                         kappa = instsupers inst 
>                         m'    = insertConstraint p m
>                         p'    = (instname inst, instparameters inst)
>                         wrap (n,x) = (n,[x])


Inserting a new constraint in a constraint map

> insertConstraint :: Pred Name -> ConstraintMap -> ConstraintMap
> insertConstraint p@(n,_) m = case Map.lookup n m of
>                                   Nothing -> Map.insert n [p] m
>                                   Just ps -> Map.insert n ([p] `union` ps) m


> (+:) :: SatSubst -> SatSubst -> SatSubst
> s1 +: s2 = Map.unionWith union s1 s2


> someUnify :: Pred Name -> [Inst Name] -> TcM Bool
> someUnify p insts = anyM (unify' p) insts
>                     where
>                        unify' p inst = let p' = (instname inst, 
>                                                  instparameters inst) 
>                                          in handle (mgu p p') True False

> unsatisfiability :: Pred Name -> String
> unsatisfiability p = "Could not satisfy the following constraint:\n" ++ 
>                      (show $ pprint p)            

> anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
> anyM p []     = return False
> anyM p (x:xs) = liftM2 (||) (p x) (anyM p xs)

> allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
> allM p []     = return True
> allM p (x:xs) = liftM2 (&&) (p x) (allM p xs)

> handle :: MonadError e m => m a -> b -> b -> m b
> handle m b1 b2 = do {
>                      m ;
>                      return b1
>                  } `catchError` (const (return b2))


A function for test if a list is a subset of another

> subseteq :: Eq a => [a] -> [a] -> Bool
> subseteq xs ys = foldr ((&&)  .  flip elem ys) True xs


A function that test if the type ordering is valid

> allLessThan :: Pred Name -> ConstraintMap -> TcM Bool
> allLessThan p@(c,_) m = do
>                           let 
>                               visited = maybe [] id (Map.lookup c m)
>                           allM (p <:) visited

Check if a constraint was visited before.

> wasVisited :: Pred Name -> ConstraintMap -> TcM Bool
> wasVisited p@(n, _) m = return (maybe False (p `elem`) (Map.lookup n m))


Convert a Satsust in a TySubst

> toTySubst :: SatSubst -> TySubst Name
> toTySubst = map unwrap . filter isSingle . Map.toList
>             where
>                 isSingle (_, [x]) = True
>                 isSingle  _  = False
>                 unwrap (n,[x]) = (n,x)

