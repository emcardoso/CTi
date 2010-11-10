This file contains the whole compiler pipeline function

> module CompilerPipeline where

> import Data.Map (showTree)

> import Haskell.Lexer.Lexer
> import Haskell.Parser.Parser
> import Haskell.Pretty.Pretty
> import Haskell.Syntax.Syntax

> import System.Console.CmdArgs.Implicit
> import System.Environment

> import Desugar.HsSynDesugar
> import Desugar.Desugar hiding (emptyEnv)

> import Tc.Kc.KcModule
> import Tc.Kc.KcBase hiding (emptyEnv)
> import Tc.TcModule
> import Tc.TcMonad
> import Tc.Assumption
> import Tc.TcLoadTyContext
> import Tc.Utils.Nameable
> import Tc.TcSignatures

> import Utils.CompilerConfig
> import Utils.Pretty


> compile :: FilePath -> IO ()
> compile file = case parseModule file of {
>                      (ParseFailed _ err) -> error err ; 
>                      (ParseOk m)         -> startCompilation m 
>                }

> startCompilation :: HsModule -> IO ()
> startCompilation m@(HsModule _ _ exps imps _) 
>               = do
>                   let files = map toFileName imps
>                       toFileName = undefined
>                   startPipeline m


> startPipeline :: HsModule -> IO ()
> startPipeline m = do 
>                       r  <- desugarer m
>                       case r of
>                           Right (cs, is, as, cis, bs) 
>                                  -> do 
>                                       let 
>                                           ks = mkKindGroups cs as bs
>                                       e <- kc ks
>                                       either error (const $ putStrLn "") e
>                                       let
>                                          (cs', as', sigs') = unkindable e
>                                          bs' = addNewSigs sigs' bs
>                                       e' <- tc (mkContext cs' as') cis bs'
>                                       either error (\x -> putStrLn "*** Types Infered ***\n" >> showTys x) e'
>                           Left er -> error er

> showTys = putStrLn . unlines . map (show . pprint) 
