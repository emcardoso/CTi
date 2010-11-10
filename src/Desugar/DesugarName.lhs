In this module we just convert the Names.

> module Desugar.DesugarName where

> import Control.Monad
> import Haskell.Syntax.Syntax

> import Desugar.Desugar

> import qualified Syntax.NameSyn as Syn

> desugarHsQName :: HsQName -> DesugarM Syn.Name
> desugarHsQName (Qual m n)  = liftM2 Syn.Qual (remModule m) (remName n)
> desugarHsQName (UnQual n)  = liftM Syn.Unqual (remName n)
> desugarHsQName (Special s) = desugarSpecial s

> desugarHsName :: HsName -> DesugarM Syn.Name
> desugarHsName n = liftM Syn.Unqual (remName n)

> desugarHsQOp :: HsQOp -> DesugarM Syn.Name
> desugarHsQOp (HsQVarOp n) = desugarHsQName n
> desugarHsQOp (HsQConOp n) = desugarHsQName n

> remName :: HsName -> DesugarM String
> remName (HsIdent s)  = return s
> remName (HsSymbol s) = return s

> remModule :: Module -> DesugarM String
> remModule (Module m) = return m

> desugarSpecial :: HsSpecialCon -> DesugarM Syn.Name
> desugarSpecial HsCons         = return Syn.ConsCon
> desugarSpecial HsListCon      = return Syn.ListCon
> desugarSpecial HsFunCon       = return Syn.ArrowCon
> desugarSpecial (HsTupleCon n) = return $ Syn.TupleCon n
> desugarSpecial HsUnitCon      = return $ Syn.TupleCon 0
