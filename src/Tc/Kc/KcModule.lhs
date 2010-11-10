> {-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}

This module does the kind inference for classes and data types 
declarations. At this point, type synonyms have been expanded.

> module Tc.Kc.KcModule where

> import Control.Monad
> import Control.Monad.Trans
> import Control.Monad.Error (throwError)
> import Control.Monad.State

> import Data.List(partition)
> import qualified Data.Map as Map

> import Syntax.CoreSyn

> import Tc.Class
> import Tc.Assumption

> import Tc.Kc.KcBase

> import Tc.Utils.DependAnalysis
> import Tc.Utils.Nameable

> import Utils.Pretty



> kc :: [Kindable Name] -> IO (Either String [Kindable Name])
> kc ds = do 
>            (e,k) <- runKcM (kcModule (group ds))
>            case e of
>               Left err -> return $ Left err
>               Right _  -> liftM Right $ applyInferedKinds (env k) ds
                               


> kcModule :: [[Kindable Name]] -> KcM ()
> kcModule ks = do 
>                   mapM_ kcGroup ks
>                   defaultToStar

> kcGroup :: [Kindable Name] -> KcM ()
> kcGroup ds = do
>                 let (cs,x) = partition isClassDecl ds
>                     (tys,sigs) = partition isConDecl x
>                 mapM_ kcClassDef cs
>                 ks <- mapM kcDataCon tys
>                 mapM_ (flip unify Star) ks
>                 ksigs <- mapM kcSig sigs
>                 mapM_ (flip unify Star) ksigs
>                 applyCurrentSubst


> kcSig :: Kindable Name -> KcM Kind
> kcSig (TyScheme (_ :>: (Forall (ps :=> ty)))) = do
>                                                  mapM_ (kcContext False) ps
>                                                  kcTy False ty

> kcDataCon :: Kindable Name -> KcM Kind 
> kcDataCon (TyAssump (n :>: t@(Forall (ps :=> ty)))) = do
>                                                           mapM_ (kcContext False) ps
>                                                           kcTy False ty

> kcClassDef :: Kindable Name -> KcM ()
> kcClassDef (ClassDef c) = do
>                               vs <- mapM newNameKindVar (parameters c)
>                               extendEnv vs
>                               kcClassContext (supers c)
>                               mapM_ kcClassMember (tyAssumpWrap $ members c)
>                               mapM_ (kcInst (map snd vs)) (instances c)


> kcClassContext :: [(Name, [TyVar Name])] -> KcM ()
> kcClassContext bs = mapM_ (kcContext False) (map context' bs)
>                     where 
>                        context' (n,ts) = (n, map TVar ts)

> kcInst :: [Kind] -> Inst Name -> KcM ()
> kcInst ks i = do
>                   kcInstContext (instsupers i)
>                   ks' <- mapM (kcTy False) (instparameters i)
>                   zipWithM_ unify ks ks'
>

> kcInstContext :: [Pred Name] -> KcM ()
> kcInstContext = mapM_ (kcContext False)

> kcClassMember :: Kindable Name -> KcM ()
> kcClassMember (TyAssump (n :>: (Forall (ps :=> ty)))) 
>                               = do
>                                   mapM_ (kcContext True) ps
>                                   kcTy False ty
>                                   return ()


> kcTy :: Bool -> Ty Name -> KcM Kind
> kcTy varExist (TCon t) = do
>                             e <- gets env
>                             k <- lookupKind (TyConDef t)
>                             case k of
>                                Nothing -> case varExist of 
>                                               True -> throwError ("Could not find a kind for:\n" ++ (show $ pprint t))
>                                               False -> do
>                                                           k' <- newKindVar 
>                                                           addInKindEnv (TyConDef t) k'
>                                                           return k'
>                                Just k' -> return k'
> kcTy varExist (TVar v) = do
>                           e <- gets env
>                           k <- lookupKind (TyVarDef v)
>                           case k of
>                               Nothing -> case varExist of
>                                               True  -> throwError ("Could not find a kind for:\n" ++ 
>                                                                         (show $ pprint v))
>                                               False -> do 
>                                                           (v, k') <- newNameKindVar v
>                                                           addInKindEnv v k'
>                                                           return k'
>                               Just k' -> return k'
> kcTy x (TyApp t1 t2) = do
>                           k1 <- kcTy x t1
>                           k2 <- kcTy x t2
>                           kn <- newKindVar
>                           unify k1 (k2 `KFun` kn)
>                           return kn
> kcTy x (TyFun t1 t2) = do
>                            k1 <- kcTy x t1
>                            k2 <- kcTy x t2
>                            unify k1 Star
>                            unify k2 Star
>                            return Star
> kcTy x (TyTuple ts) = do
>                           ks <- mapM (kcTy x) ts
>                           mapM_ (unify Star) ks
>                           return Star
> kcTy x (TyList t) = do
>                         k <- kcTy x t
>                         unify k Star
>                         return k


> kcContext :: Bool -> (Name, [Ty Name]) -> KcM ()
> kcContext b (n,ts) = mapM_ (kcTy b) ts



> newNameKindVar :: TyVar a -> KcM (Kindable a, Kind)
> newNameKindVar v = do
>                       k <- newKindVar
>                       return (TyVarDef v, k)


Here we do a function to apply infered kinds

> applyInferedKinds :: Map.Map (Kindable Name) Kind -> [Kindable Name] -> IO [Kindable Name]
> applyInferedKinds kc = mapM (applyInferedKind kc)


> applyInferedKind :: Map.Map (Kindable Name) Kind -> Kindable Name -> IO (Kindable Name)                       
> applyInferedKind kc (ClassDef c) = liftM ClassDef (applyInClass kc c)
> applyInferedKind kc a@(TyAssump _) = liftM TyAssump (applyInAssump kc (tyAssump a))
> applyInferedKind kc t@(TyConDef _) = applyInTyCon kc t
> applyInferedKind kc v@(TyVarDef _) = applyInTyVar kc v
> applyInferedKind kc t@(TyScheme _) = liftM TyScheme $ applyInScheme kc (tyScheme t)


> applyInClass :: Map.Map (Kindable Name) Kind -> Class Name -> IO (Class Name)
> applyInClass kc c = do
>                       ts <- mapM (applyInTyVar kc . TyVarDef) (parameters c)
>                       ss <- mapM (applyInSup kc) (supers c)
>                       as <- mapM (applyInAssump kc) (members c)
>                       is <- mapM (applyInInst kc) (instances c)
>                       return (Class (name c) (map tyVarDef ts) ss as is)

> applyInTyVar :: Map.Map (Kindable Name) Kind -> Kindable Name -> IO (Kindable Name)
> applyInTyVar kc v@(TyVarDef (Bound n _)) = case Map.lookup v kc of
>                                                   Just k  -> return (TyVarDef (Bound n k))
>                                                   Nothing -> do
>                                                                   error (concat ["Could not find the kind infered for\n",
>                                                                                   show $ pprint n])


> applyInSup :: Map.Map (Kindable Name) Kind -> (Name, [TyVar Name]) -> IO (Name, [TyVar Name])
> applyInSup kc (n,ts) = liftM (((,) n) . map tyVarDef) $ mapM (applyInTyVar kc . TyVarDef) ts

> applyInPred :: Map.Map (Kindable Name) Kind -> Pred Name -> IO (Pred Name)
> applyInPred kc (n,ts) = liftM ((,) n) $ mapM (applyInTy kc) ts


> applyInAssump :: Map.Map (Kindable Name) Kind -> Assumption Name -> IO (Assumption Name)
> applyInAssump kc (n :>: qt) = applyInScheme kc (n :>: qt)


> applyInScheme :: Map.Map (Kindable Name) Kind -> Assumption Name -> IO (Assumption Name)
> applyInScheme kc (n :>: (Forall (ps :=> ty))) 
>           = do
>                ps' <- mapM (applyInPred kc) ps
>                ty' <- applyInTy kc ty
>                return (n :>: (Forall (ps' :=> ty')))

> applyInTy :: Map.Map (Kindable Name) Kind -> Ty Name -> IO (Ty Name)
> applyInTy kc (TVar v) = liftM (TVar . tyVarDef) $ applyInTyVar kc (TyVarDef v)
> applyInTy kc (TCon t) = liftM (TCon . tyConDef) $ applyInTyCon kc (TyConDef t)
> applyInTy kc (TyFun l r) = liftM2 TyFun (applyInTy kc l) (applyInTy kc r)
> applyInTy kc (TyApp l r) = liftM2 TyApp (applyInTy kc l) (applyInTy kc r)
> applyInTy kc (TyTuple ts) = liftM TyTuple $ mapM (applyInTy kc) ts
> applyInTy kc (TyList t) = liftM TyList (applyInTy kc t)


> applyInTyCon :: Map.Map (Kindable Name) Kind -> Kindable Name -> IO (Kindable Name)
> applyInTyCon kc t@(TyConDef (TyCon n _)) = case Map.lookup t kc of
>                                                  Just k  -> return $ TyConDef (TyCon n k)
>                                                  Nothing ->  error (concat ["Could not find the kind infered for\n",
>                                                                                   show $ pprint n])

> applyInInst :: Map.Map (Kindable Name) Kind -> Inst Name -> IO (Inst Name)
> applyInInst kc i = do
>                      tys <- mapM (applyInTy kc) (instparameters i) 
>                      ss  <- mapM (applyInPred kc) (instsupers i)
>                      return (Inst (instname i) tys ss)

The first thing to be done is to build the dependencies groups

> group :: (HasName a Name, Show (a Name)) => [a Name] -> [[a Name]] 
> group ds = getDependentGroups ds (getName KindAnalysis) (flip getReferencedNames (map (getName KindAnalysis) ds))


> isClassDecl (ClassDef _) = True
> isClassDecl _            = False

> isConDecl (TyAssump _) = True
> isConDecl _            = False
