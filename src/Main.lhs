> module Main where

> import System.Console.CmdArgs.Implicit
> import System.Environment
> import Prelude.PreludeBuiltIn


> import CompilerPipeline
> import Utils.CompilerConfig

> import Tc.TcExpr -- ALT
> import Syntax.TySyn
> import Syntax.NameSyn

> defaultCfg = Config { displayCore = def &= help "Display desugared code", 
>                       outputFile  = def &= help "Name of file to store desugared code" }
>             

> testId :: Ty Name
> testId = TyFun (TVar (Free 0 Star))  (TVar (Free 0 Star)) 

> testAnd :: Ty Name
> testAnd = TyFun (TyAnd [TyFun tInt (TVar (Free 0 Star)), TyFun tBool (TVar (Free 1 Star)) ]) 
>                (TyFun (TVar (Free 0 Star)) ( TyTuple [ TVar (Free 0 Star),  TVar (Free 1 Star)] ) )


> main = do
>           (f:_) <- getArgs
>           s <- readFile f
>           compile s

