This file defines a generic environment that is used in several phases
in the compiler. 

> module Utils.Env(Env, insert, lookup, null, empty, names, toList, fromList, elems, unEnv) where

> import qualified Data.Map as Map
> import Data.Functor
> import Prelude hiding (lookup, null)

> import Syntax.NameSyn

the definition of the environment

> newtype Env a = Env { unEnv :: Map.Map Name a } deriving (Eq, Ord, Show)

creating an empty environment

> empty :: Env a
> empty = Env Map.empty

verifying if a environment is null

> null :: Env a -> Bool
> null = Map.null . unEnv

inserting a value

> insert :: Ord a => Name -> a -> Env a -> Env a
> insert n v = Env . Map.insert n v . unEnv

creating a singleton environment

> singleton :: Ord a => Name -> a -> Env a
> singleton n = flip (insert n) empty

looking for a value in the environment

> lookup :: Ord a => Name -> Env a -> Maybe a
> lookup n = Map.lookup n . unEnv

verifying if a key is in the environment

> member :: Name -> Env a -> Bool
> member n = Map.member n . unEnv

getting the size of an environment

> size :: Env a -> Int
> size = Map.size . unEnv

getting the names of an environment 

> names :: Env a -> [Name]
> names = Map.keys . unEnv

> elems :: Env a -> [a]
> elems = Map.elems . unEnv

Functor instance

> instance Functor Env where
>   fmap f = Env . fmap f . unEnv

converting to / from list 

> fromList :: [(Name, a)] -> Env a
> fromList = Env . Map.fromList

> toList :: Env a -> [(Name, a)]
> toList = Map.toList . unEnv

