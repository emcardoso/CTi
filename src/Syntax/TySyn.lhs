> {-# LANGUAGE TypeSynonymInstances, FlexibleContexts, DeriveDataTypeable #-}

This file contains the syntax of types 

> module Syntax.TySyn where

> import Data.Generics hiding (TyCon)

> import Utils.Pretty

Data type declaration

> data Ty a = TVar (TyVar a)
>           | TCon (TyCon a)
>           | TyApp (Ty a) (Ty a)
>           | TyFun (Ty a) (Ty a)    
>           | TyTuple [Ty a]         -- Unit type is TyTuple []
>           | TyList (Ty a)
>           | TyAnd [Ty a]           -- ALT: Added to support Insersection Type annotation.
>           deriving(Eq, Ord, Show, Data, Typeable)


Type variables

> data TyVar a = Bound a Kind 
>              | Free Int Kind               -- this is used by the type inference engine
>              deriving (Eq, Ord, Show, Data, Typeable)


Type construtors

> data TyCon a = TyCon a Kind 
>                deriving (Show, Data, Typeable)

We compare type constructos based only in his name.

> instance Eq a => Eq (TyCon a) where
>   (TyCon n _) == (TyCon n' _) = n == n'

> instance Ord a => Ord (TyCon a) where
>   (TyCon n _) <= (TyCon n' _) = n <= n'

Kinds 

> data Kind = Star | KFun Kind Kind | KVar KindVar
>             deriving (Eq, Ord, Show, Data, Typeable)

> class HasKind t where
>     kind :: t -> Kind

> instance HasKind (TyVar a) where
>     kind (Bound _ k) = k
>     kind (Free _  k) = k

> instance HasKind (TyCon a) where
>     kind (TyCon _ k) = k

> instance HasKind (Ty a) where
>     kind (TVar v)    = kind v
>     kind (TCon t)    = kind t
>     kind (TyApp t _) = case kind t of
>                           (KFun _ k) -> k
>     kind _           = Star


> newtype KindVar = KindVar Int deriving (Eq, Ord, Show, Data, Typeable)

Class Predicates

> type Pred a = (a, [Ty a])

Qualified types

> data Qual a = [Pred a] :=> (Ty a)
>               deriving (Eq, Ord, Show, Data, Typeable)

Type Schemes

> data Scheme a = Forall (Qual a)
>                 deriving (Eq, Ord, Show, Data, Typeable)

The pretty printer

> instance Pretty a => Pretty (Scheme a) where
>     pprint(Forall qt) = pprint qt

> instance (Pretty a) => Pretty (Qual a) where
>     pprint ([] :=> t) = pprint t
>     pprint (ps :=> t) = parens (pPunctuate comma ps) <+> pdarrow <+> pprint t

> instance Pretty a => Pretty (Pred a) where
>     pprint (n, ts) = pprint n <+> pUnwords ts


> instance Pretty a => Pretty (Ty a) where
>     pprint (TVar v)     = pprint v
>     pprint (TCon c)     = pprint c
>     pprint (TyApp f a)  = pprint f <+> pprint a
>     pprint (TyFun f a)  = pprint f <+> parrow <+> pprint a
>     pprint (TyTuple ts) = parens (pPunctuate comma ts)
>     pprint (TyList t)   = brackets (pprint t)

> instance Pretty a => Pretty (TyVar a) where
>     pprint (Bound s _) = pprint s
>     pprint (Free i  _) = text ("x" ++ show i)

> instance Pretty a => Pretty (TyCon a) where
>     pprint (TyCon n _)    = pprint n

> instance Pretty Kind where
>     pprint Star         = text "*"
>     pprint (KVar v)     = text ("k" ++ (show v))
>     pprint (KFun k1 k2) = pprint k1 <+> parrow <+> pprint k2
