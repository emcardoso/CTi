> {-# LANGUAGE MultiParamTypeClasses #-}

This file contains the main definitions for the kind inference / checking
algorithm.

> module Tc.Kc.KcBase where

> import Control.Monad.Error
> import Control.Monad.State

> import Data.Generics hiding (TyCon)
> import Data.List(intersect)
> import Data.Maybe(mapMaybe)

> import Prelude.PreludeBuiltIn

> import Syntax.CoreSyn

> import Tc.Assumption
> import Tc.Class
> import Tc.Kc.KcSubst
> import Tc.Utils.Nameable

> import qualified Data.Map as Map
> import Utils.Pretty hiding (empty, float, char, int)



a environment to the kind inference process 

> data KcEnv = KcEnv {
>                 env   :: Map.Map (Kindable Name) Kind,  -- kind environment
>                 subst :: KindSubst,                     -- current substitution
>                 fresh :: Int                            -- fresh var supply
>              } deriving (Eq, Ord, Show)

> instance Kinds KcEnv where
>    vars      = map unvar . filter isVar . Map.elems . env
>                where isVar (KVar _) = True
>                      isVar _        = False
>                      unvar (KVar v) = v
>    apply s e = e {env = apply1 s (env e)}

> apply1 s = Map.fromList . map f . Map.toList
>            where
>               f (n,k) = (n, apply s k)

!!!!!TODO FIX this!!!! This cannot stay here. Just for testing .

An empty environment

> emptyEnv = KcEnv (Map.fromList kindtable') nullSubst 0
>            where
>               kindtable' = map (\t -> (TyConDef t, kind t)) kindtable

The monad for the kind inference properly

> type KcM a = ErrorT String (StateT KcEnv IO) a


Some monad operations

> newKindVar :: KcM Kind
> newKindVar = do
>                 v <- gets fresh
>                 incVar
>                 return (KVar $ KindVar v) 


> incVar :: KcM ()
> incVar = do
>            e <- get
>            put (e{fresh = 1 + (fresh e)})

> getSubst :: KcM KindSubst
> getSubst = gets subst


> extSubst :: KindSubst -> KcM ()
> extSubst s = do 
>                e <- get
>                put (e{subst = s @@ (subst e)})


> lookupKind :: Kindable Name -> KcM (Maybe Kind)
> lookupKind n = liftM (Map.lookup n) (gets env)


> addInKindEnv :: Kindable Name -> Kind -> KcM ()
> addInKindEnv n k = modify (putInEnv n k)
>                    where
>                       putInEnv n k e = e{env = Map.insert n k (env e)}

> extendEnv :: [(Kindable Name, Kind)] -> KcM ()
> extendEnv = mapM_ (uncurry addInKindEnv)

> applyCurrentSubst :: KcM ()
> applyCurrentSubst = do
>                       s <- getSubst
>                       e <- get
>                       put (apply s e)


> defaultToStar :: KcM ()
> defaultToStar = do
>                   e <- get
>                   kvs <- gets (vars . Map.elems . env)
>                   put (apply (zip kvs (repeat Star)) e)

Unification Algorithm

> mgu :: Kind -> Kind -> KcM KindSubst
> mgu Star Star = return nullSubst
> mgu (KFun k1 k2) (KFun k3 k4) = do
>                                   s1 <- mgu k1 k3
>                                   s2 <- mgu (apply s1 k2)
>                                             (apply s1 k4)
>                                   return (s2 @@ s1)
> mgu (KVar u) k = varBind u k
> mgu k (KVar u) = varBind u k
> mgu k1 k2      = throwError ("Kind Inference Error\nCould not unify:\n" ++ (show $ pprint k1) ++ "\nwith\n"
>                                                                         ++ (show $ pprint k2))


> varBind :: KindVar -> Kind -> KcM KindSubst 
> varBind v k
>         | k == KVar v     = return nullSubst
>         | v `elem` vars k = throwError "Kind Inference Error\nOccurs check error"
>         | otherwise       = return [(v,k)]


> unify :: Kind -> Kind -> KcM ()
> unify k1 k2 = do 
>                  s  <- getSubst
>                  s' <- mgu (apply s k1) (apply s k2)
>                  extSubst s'

Here we define a data type for wrap thing that must pass to kind checking process

> data Kindable a = ClassDef {classDef :: Class a}
>                 | TyAssump {tyAssump :: Assumption a} 
>                 | TyConDef {tyConDef :: TyCon a}
>                 | TyVarDef {tyVarDef :: TyVar a}
>                 | TyScheme {tyScheme :: Assumption a}
>                 deriving (Eq, Ord)

> instance Pretty a => Show (Kindable a) where
>    show (ClassDef c) = show $ pprint c
>    show (TyAssump a) = show $ pprint a
>    show (TyConDef t) = show $ pprint t
>    show (TyVarDef a) = show $ pprint a
>    show (TyScheme t) = show $ pprint t

> classWrap :: [Class a] -> [Kindable a]
> classWrap = map ClassDef

> tyAssumpWrap :: [Assumption a] -> [Kindable a]
> tyAssumpWrap = map TyAssump


> instance HasName Kindable Name where
>       getName _ (ClassDef c)                  = getName KindAnalysis c
>       getName _ (TyAssump t)                  = getName KindAnalysis t
>       getName _ (TyScheme (n :>: _))          = n

>       getReferencedNames (ClassDef c)                  = getReferencedNames c
>       getReferencedNames (TyAssump c)                  = getReferencedNames c
>       getReferencedNames (TyScheme t) = getReferencedNames t


this function does the initialization of the data that will be kind-checked

> mkKindGroups cs as bs = classWrap cs ++ tyAssumpWrap as ++ (tySigWrap bs)
>                         where tySigWrap = map TyScheme . mapMaybe getScheme
>                               getScheme (FunBind n (Just sig) _) = Just (n :>: sig)
>                               getScheme _                        = Nothing


Here we just separate the results of the kind inference process

> unkindable (Right ks) = (cs, as, sigs)
>                         where 
>                          cs   = map classDef $ filter isClassDef ks
>                          as   = map tyAssump $ filter isTyAssump ks
>                          sigs = map tyScheme $ filter isTyScheme ks
>                          isClassDef (ClassDef _) = True
>                          isClassDef _            = False
>                          isTyAssump (TyAssump _) = True
>                          isTyAssump _            = False
>                          isTyScheme (TyScheme _) = True
>                          isTyScheme _            = False


                                   

Running the kind inference

> runKcM :: KcM a -> IO (Either String a, KcEnv)
> runKcM k = do 
>                r <- runStateT (runErrorT k) emptyEnv
>                case r of
>                   (Left err, _) -> do 
>                                      putStrLn err
>                                      return r
>                   (Right _, e)  -> return r
