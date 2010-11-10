-----------------------------------------------------------------------------
--
-- Module      :  Language.Haskell.Syntax.AnnotatedSyntax
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Abstract syntax for haskell that allows qualified names everywhere.
--   used for renaming phase of the compilation.
--
-----------------------------------------------------------------------------

module Haskell.Syntax.AnnotatedSyntax where

import Haskell.Syntax.Syntax (SrcLoc (..))
import Haskell.Pretty.Pretty

-- Annotated module definition

newtype AModule = AModule String
                  deriving (Eq,Ord,Show)


bogusASrcLoc = SrcLoc "" (-1) (-1)

-- Annotated special constructors, these are never qualified.

data AHsSpecialCon
	= AHsUnitCon		-- ^ unit type and data constructor @()@
	| AHsListCon		-- ^ list type constructor @[]@
	| AHsFunCon		-- ^ function type constructor @->@
	| AHsTupleCon Int	-- ^ /n/-ary tuple type and data
				--   constructors @(,)@ etc
	| AHsCons		-- ^ list data constructor @(:)@
    deriving (Eq, Ord, Show)

-- Annotated names and operators

data AHsName
	= AQual AModule AHsIdentifier
	| AUnQual AHsIdentifier
	| AHsSpecial AHsSpecialCon
    deriving (Eq, Ord, Show)

data AHsIdentifier
     = AHsIdent String
     | AHsSymbol String
     deriving (Eq,Ord, Show)

data AHsOp = AHsQVarOp AHsName	-- ^ variable operator (/qvarop/)
	       | AHsQConOp AHsName	-- ^ constructor operator (/qconop/)
	       deriving (Eq, Ord, Show)
	
data AHsCName = AHsVarName AHsName	-- ^ name of a method or field
	          | AHsConName AHsName	-- ^ name of a data constructor
	          deriving (Eq, Ord, Show)

-- Annotated module definition

data AHsModule = AHsModule SrcLoc AModule (Maybe [AHsExportSpec])
                         [AHsImportDecl] [AHsDecl]
               deriving (Eq, Ord, Show)

-- Export especification

data AHsExportSpec
	 = AHsEVar AHsName			-- variable
	 | AHsEAbs AHsName			-- T
	 | AHsEThingAll AHsName			-- T(..)
	 | AHsEThingWith AHsName [AHsCName]	-- T(C_1,...,C_n)
	 | AHsEModuleContents AModule		-- module M   (not for imports)
  deriving (Eq, Ord, Show)


-- Import declaration

data AHsImportDecl = AHsImportDecl {
                       importLoc :: SrcLoc		    -- ^ position of the @import@ keyword.
	                 , importModule :: AModule	    -- ^ name of the module imported.
	                 , importQualified :: Bool	    -- ^ imported @qualified@?
	                 , importAs :: Maybe AModule	-- ^ optional alias name in an
					                                -- @as@ clause.
	                 , importSpecs :: Maybe (Bool,[AHsImportSpec])
			            -- ^ optional list of import specifications.
			            -- The 'Bool' is 'True' if the names are excluded
			            -- by @hiding@.
	                } deriving (Eq, Ord, Show)
	
-- | Import specification.
data AHsImportSpec
	 = AHsIVar AHsName			-- ^ variable
	 | AHsIAbs AHsName			-- ^ @T@:
			-- the name of a class, datatype or type synonym.
	 | AHsIThingAll AHsName			-- ^ @T(..)@:
			-- a class imported with all of its methods, or
			-- a datatype imported with all of its constructors.
	 | AHsIThingWith AHsName [AHsCName]	-- ^ @T(C_1,...,C_n)@:
			-- a class imported with some of its methods, or
			-- a datatype imported with some of its constructors.
     deriving (Eq, Ord, Show)
     	
-- | Associativity of an operator.
data AHsAssoc
	 = AHsAssocNone	-- ^ non-associative operator (declared with @infix@)
	 | AHsAssocLeft	-- ^ left-associative operator (declared with @infixl@).
	 | AHsAssocRight	-- ^ right-associative operator (declared with @infixr@)
	 deriving (Eq, Ord, Show)
			
-- Declarations

data AHsDecl
	 = AHsTypeDecl	 SrcLoc AHsName [AHsName] AHsType
	 | AHsDataDecl	 SrcLoc AHsContext AHsName [AHsName] [AHsConDecl] [AHsName]
	 | AHsInfixDecl   SrcLoc AHsAssoc Int [AHsOp]
	 | AHsNewTypeDecl SrcLoc AHsContext AHsName [AHsName] AHsConDecl [AHsName]
	 | AHsClassDecl	 SrcLoc AHsContext AHsName [AHsName] [AHsDecl]
	 | AHsInstDecl	 SrcLoc AHsContext AHsName [AHsType] [AHsDecl]
	 | AHsDefaultDecl SrcLoc [AHsType]
	 | AHsTypeSig	 SrcLoc [AHsName] AHsQualType
	 | AHsFunBind     [AHsMatch]
	 | AHsPatBind	 SrcLoc AHsPat AHsRhs {-where-} [AHsDecl]
	 deriving (Eq, Ord, Show)
	
-- | Clauses of a function binding.
data AHsMatch
	 = AHsMatch SrcLoc AHsName [AHsPat] AHsRhs {-where-} [AHsDecl]
	 deriving (Eq, Ord, Show)
	
-- | Declaration of a data constructor.

data AHsConDecl
	 = AHsConDecl SrcLoc AHsName [AHsBangType]
				-- ^ ordinary data constructor
	 | AHsRecDecl SrcLoc AHsName [([AHsName],AHsBangType)]
     deriving (Eq, Ord, Show)

-- A type with strict markers

data AHsBangType
	 = AHsBangedTy   AHsType	-- ^ strict component, marked with \"@!@\"
	 | AHsUnBangedTy AHsType	-- ^ non-strict component
	 deriving (Eq, Ord, Show)

-- | The right hand side of a function or pattern binding.
data AHsRhs
	 = AHsUnGuardedRhs AHsExp	-- ^ unguarded right hand side (/exp/)
	 | AHsGuardedRhss  [AHsGuardedRhs]
     deriving (Eq, Ord, Show)

-- | A guarded right hand side @|@ /exp/ @=@ /exp/.
-- The first expression will be Boolean-valued.
data AHsGuardedRhs
	 = AHsGuardedRhs SrcLoc AHsExp AHsExp
	   deriving (Eq, Ord, Show)
	
-- | A type qualified with a context.
--   An unqualified type has an empty context.
data AHsQualType
	 = AHsQualType AHsContext AHsType
	   deriving (Eq, Ord, Show)

-- | Haskell types and type constructors.
data AHsType
	 = AHsTyFun   AHsType AHsType	-- ^ function type
	 | AHsTyTuple [AHsType]		-- ^ tuple type
	 | AHsTyApp   AHsType AHsType	-- ^ application of a type constructor
	 | AHsTyAnd  [AHsType]          -- ^ Tipos Interseção
	 | AHsTyVar   AHsName		-- ^ type variable
	 | AHsTyCon   AHsName		-- ^ named type or type constructor
	 deriving (Eq, Ord, Show)
	
type AHsContext = [AHsAsst]

-- | Class assertions.
--   In Haskell 98, the argument would be a /tyvar/, but this definition
--   allows multiple parameters, and allows them to be /type/s.
type AHsAsst    = (AHsName,[AHsType])


-- | /literal/.
-- Values of this type hold the abstract value of the literal, not the
-- precise string representation used.  For example, @10@, @0o12@ and @0xa@
-- have the same representation.
data AHsLiteral
	= AHsChar	Char		-- ^ character literal
	| AHsString	String		-- ^ string literal
	| AHsInt		Integer		-- ^ integer literal
	| AHsFrac	Rational	-- ^ floating point literal
	| AHsCharPrim	Char		-- ^ GHC unboxed character literal
	| AHsStringPrim	String		-- ^ GHC unboxed string literal
	| AHsIntPrim	Integer		-- ^ GHC unboxed integer literal
	| AHsFloatPrim	Rational	-- ^ GHC unboxed float literal
	| AHsDoublePrim	Rational	-- ^ GHC unboxed double literal
	deriving(Eq, Ord, Show)
	
	
data AHsExp
	= AHsVar AHsName			-- ^ variable
	| AHsCon AHsName			-- ^ data constructor
	| AHsLit AHsLiteral		-- ^ literal constant
	| AHsInfixApp AHsExp AHsOp AHsExp	-- ^ infix application
	| AHsApp AHsExp AHsExp		-- ^ ordinary application
	| AHsNegApp AHsExp		-- ^ negation expression @-@ /exp/
	| AHsLambda SrcLoc [AHsPat] AHsExp -- ^ lambda expression
	| AHsLet [AHsDecl] AHsExp		-- ^ local declarations with @let@
	| AHsIf AHsExp AHsExp AHsExp	-- ^ @if@ /exp/ @then@ /exp/ @else@ /exp/
	| AHsCase AHsExp [AHsAlt]		-- ^ @case@ /exp/ @of@ /alts/
	| AHsDo [AHsStmt]			-- ^ @do@-expression:
					-- the last statement in the list
					-- should be an expression.
	| AHsTuple [AHsExp]		-- ^ tuple expression
	| AHsList [AHsExp]		-- ^ list expression
	| AHsParen AHsExp			-- ^ parenthesized expression
	| AHsLeftSection AHsExp AHsOp	-- ^ left section @(@/exp/ /qop/@)@
	| AHsRightSection AHsOp AHsExp	-- ^ right section @(@/qop/ /exp/@)@
	| AHsRecConstr AHsName [AHsFieldUpdate]
					-- ^ record construction expression
	| AHsRecUpdate AHsExp [AHsFieldUpdate]
					-- ^ record update expression
	| AHsEnumFrom AHsExp		-- ^ unbounded arithmetic sequence,
					-- incrementing by 1
	| AHsEnumFromTo AHsExp AHsExp	-- ^ bounded arithmetic sequence,
					-- incrementing by 1
	| AHsEnumFromThen AHsExp AHsExp	-- ^ unbounded arithmetic sequence,
					-- with first two elements given
	| AHsEnumFromThenTo AHsExp AHsExp AHsExp
					-- ^ bounded arithmetic sequence,
					-- with first two elements given
	| AHsListComp AHsExp [AHsStmt]	-- ^ list comprehension
	| AHsExpTypeSig SrcLoc AHsExp AHsQualType
					-- ^ expression type signature
	| AHsAsPat AHsName AHsExp		-- ^ patterns only
	| AHsWildCard			-- ^ patterns only
	| AHsIrrPat AHsExp		-- ^ patterns only
    deriving (Eq, Ord, Show)


-- | A pattern, to be matched against a value.
data AHsPat
	= AHsPVar AHsName			-- ^ variable
	| AHsPLit AHsLiteral		-- ^ literal constant
	| AHsPNeg AHsPat			-- ^ negated pattern
	| AHsPInfixApp AHsPat AHsName AHsPat
					-- ^ pattern with infix data constructor
	| AHsPApp AHsName [AHsPat]	-- ^ data constructor and argument
					-- patterns
	| AHsPTuple [AHsPat]		-- ^ tuple pattern
	| AHsPList [AHsPat]		-- ^ list pattern
	| AHsPParen AHsPat		-- ^ parenthesized pattern
	| AHsPRec AHsName [AHsPatField]	-- ^ labelled pattern
	| AHsPAsPat AHsName AHsPat		-- ^ @\@@-pattern
	| AHsPWildCard			-- ^ wildcard pattern (@_@)
	| AHsPIrrPat AHsPat		-- ^ irrefutable pattern (@~@)
	deriving (Eq, Ord, Show)
	
data AHsPatField
	= AHsPFieldPat AHsName AHsPat
	deriving (Eq, Ord, Show)
	
data AHsStmt
	= AHsGenerator SrcLoc AHsPat AHsExp
				-- ^ a generator /pat/ @<-@ /exp/
	| AHsQualifier AHsExp	-- ^ an /exp/ by itself: in a @do@-expression,
				-- an action whose result is discarded;
				-- in a list comprehension, a guard expression
	| AHsLetStmt [AHsDecl]	-- ^ local bindings
	deriving (Eq, Ord, Show)
	
-- | An /fbind/ in a labeled record construction or update expression.
data AHsFieldUpdate
	= AHsFieldUpdate AHsName AHsExp
	deriving (Eq, Ord, Show)
	
-- | An /alt/ in a @case@ expression.
data AHsAlt
	= AHsAlt SrcLoc AHsPat AHsGuardedAlts [AHsDecl]
	deriving (Eq, Ord, Show)
	
data AHsGuardedAlts
	= AHsUnGuardedAlt AHsExp		-- ^ @->@ /exp/
	| AHsGuardedAlts  [AHsGuardedAlt]	-- ^ /gdpat/
	deriving (Eq, Ord, Show)
	
data AHsGuardedAlt
	= AHsGuardedAlt SrcLoc AHsExp AHsExp
	deriving (Eq, Ord, Show)


alist_tycon	      = AHsTyCon alist_tycon_name
alist_tycon_name  = AHsSpecial AHsListCon



