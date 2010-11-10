> {-#LANGUAGE DeriveDataTypeable#-}

This module contains definitions that will be used
for defining class and instances.

> module Tc.Class where

> import Data.Generics

> import Syntax.CoreSyn 

> import Tc.Assumption

> import Utils.Pretty


> data Class a = Class {
>                 name       :: a,
>                 parameters :: [TyVar a],
>                 supers     :: [(a, [TyVar a])],
>                 members    :: [Assumption a],
>                 instances  :: [Inst a]
>              } deriving (Eq, Ord, Show, Data, Typeable)


> data Inst a = Inst {
>                instname       :: a,
>                instparameters :: [Ty a],
>                instsupers     :: [(a, [Ty a])]
>             } deriving (Eq, Ord, Show, Data, Typeable)


> data OverloadBinds = OverloadBinds {
>                         classBinds :: [(Class Name, Binds Name)],
>                         instsBinds :: [(Inst Name, Binds Name)]
>                      } deriving (Eq, Ord, Show)

Some instances for dictionaries

> instance Pretty a => Pretty (Class a) where
>     pprint c = text "class" <+> (pprint $ name c)

> instance Pretty a => Pretty (Inst a) where
>     pprint c = (text "instance") <+> ctx <+> (pprint $ instname c) <+> (pUnwords (instparameters c)) <+> (text "where") 
>                where 
>                   ctx = parens (pUnwords (instsupers c))

