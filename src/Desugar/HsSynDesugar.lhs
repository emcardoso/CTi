This file export all other modules that are involved in
desugar process.

> module Desugar.HsSynDesugar (desugarer) where

> import Control.Monad
> import Control.Monad.Trans
> import Haskell.Syntax.Syntax
> import Haskell.Pretty.Pretty(prettyPrint)

> import Desugar.Desugar
> import Desugar.DesugarLit
> import Desugar.DesugarName
> import Desugar.DesugarExpr
> import Desugar.DesugarPat
> import Desugar.DesugarTy
> import Desugar.DesugarDecl
> import Desugar.DesugarTypeSyn
> import Desugar.LiftClassInstBinds
> import Desugar.CollectDataCons

> import Renamer.RenamerSyn

> import Syntax.CoreSyn

> import Tc.Assumption
> import Tc.Class

> desugarer :: HsModule -> IO (Either String ([Class Name],      -- defined classes and instances
>                                             [Inst Name] ,      -- derived instances for defined data types
>                                             [Assumption Name], -- data constructors type assumptions
>                                             OverloadBinds,      -- class / instance desugared bindings 
>                                             Binds Name))      -- Normal function bindings
> desugarer m = liftM fst $ runDesugarM (desugarSyntax m)

> desugarSyntax :: HsModule -> DesugarM ([Class Name], [Inst Name], [Assumption Name], OverloadBinds, Binds Name)
> desugarSyntax m = do
>                      m'          <- expandTySynonyms m
>                      mx          <- liftIO $ renamer m'
>                      either throwError desugarSyn' mx

> desugarSyn' m' = do
>                      (cs, cibs)  <- desugarClassInstDefs m'
>                      (dcons, is) <- collectDataCons m'
>                      nbs         <- mapM desugarHsDecl (validDecls m') 
>                      return (cs, is, dcons, cibs, nbs)

> validDecls (HsModule _ _ _ _ ds) = concatMap transformTySigs $ filter isBind ds
>                                    where
>                                       transformTySigs (HsTypeSig l ns qt) = map (flip (HsTypeSig l) qt . (:[])) ns
>                                       transformTySigs n                 = [n]

> isBind (HsTypeSig _ _ _)   = True
> isBind (HsFunBind _)       = True
> isBind (HsPatBind _ _ _ _) = True
> isBind _                   = False
