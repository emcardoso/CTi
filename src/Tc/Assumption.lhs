> {-#LANGUAGE DeriveDataTypeable#-}

This file defines a data type for represent type assumptions

> module Tc.Assumption where

> import Data.Generics

> import Utils.Pretty

> import Syntax.TySyn
> import Syntax.NameSyn

> data Assumption a = a :>: (Scheme a) deriving (Eq, Ord, Show, Data, Typeable)


Pretty

> instance Pretty a => Pretty (Assumption a) where
>     pprint (n :>: ty) = pprint n <+> text "::" <+> pprint ty
