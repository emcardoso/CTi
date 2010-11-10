> module Main where

> import System.Console.CmdArgs.Implicit
> import System.Environment

> import CompilerPipeline
> import Utils.CompilerConfig



> defaultCfg = Config { displayCore = def &= help "Display desugared code", 
>                       outputFile  = def &= help "Name of file to store desugared code" }
>             


> main = do
>           (f:_) <- getArgs
>           s <- readFile f
>           compile s

