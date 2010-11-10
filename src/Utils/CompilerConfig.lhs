> {-#LANGUAGE DeriveDataTypeable #-}

This module contains a data type for representing the compiler configuration.

> module Utils.CompilerConfig where

We need syb because the cmdargs parser api

> import Data.Generics 

Here comes the data type for defining the basic compiler configuration

> data Config = Config { 
>                 displayCore :: Bool,         -- display output core code
>                 outputFile  :: Maybe String  -- file to output generated code
>             } deriving (Eq, Show, Ord, Data, Typeable)
