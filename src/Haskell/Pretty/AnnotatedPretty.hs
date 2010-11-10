{-#LANGUAGE TypeSynonymInstances#-}

-----------------------------------------------------------------------------
--
-- Module      :  Language.Haskell.Pretty.AnnotatedPretty
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Pretty Printting for the annotated abstract Syntax
--
-----------------------------------------------------------------------------

module Haskell.Pretty.AnnotatedPretty where

import Haskell.Syntax.AnnotatedSyntax
import Haskell.Syntax.Syntax (list_tycon, SrcLoc(..))
import Haskell.Pretty.PrettyUtils

-------------------------  Pretty-Print a Module --------------------
instance Pretty AHsModule where
	pretty (AHsModule pos m mbExports imp decls) =
		                                    markLine pos $
		                                        topLevel (ppHsModuleHeader m mbExports)
			                                             (map pretty imp ++
			                                                          map pretty decls)
			
--------------------------  Module Header ------------------------------

ppHsModuleHeader :: AModule -> Maybe [AHsExportSpec] ->  Doc
ppHsModuleHeader m mbExportList = mySep [ text "module", pretty m,
	                                      maybePP (parenList . map pretty) mbExportList,
	                                      text "where" ]

instance Pretty AModule where
	pretty (AModule modName) = text modName

instance Pretty AHsExportSpec where
	pretty (AHsEVar name)		    = pretty name
	pretty (AHsEAbs name)		    = pretty name
	pretty (AHsEThingAll name)	    = pretty name <> text "(..)"
	pretty (AHsEThingWith name nameList) =
		pretty name <> (parenList . map pretty $ nameList)
	pretty (AHsEModuleContents m)       = text "module" <+> pretty m

instance Pretty AHsImportDecl where
	pretty (AHsImportDecl pos m qual mbName mbSpecs) =
		markLine pos $
		mySep [text "import",
		       if qual then text "qualified" else empty,
		       pretty m,
		       maybePP (\m' -> text "as" <+> pretty m') mbName,
		       maybePP exports mbSpecs]
	    where
		exports (b,specList) =
			if b then text "hiding" <+> specs else specs
		    where specs = parenList . map pretty $ specList

instance Pretty AHsImportSpec where
	pretty (AHsIVar name)                = pretty name
	pretty (AHsIAbs name)                = pretty name
	pretty (AHsIThingAll name)           = pretty name <> text "(..)"
	pretty (AHsIThingWith name nameList) =
		pretty name <> (parenList . map pretty $ nameList)
		
-------------------------  Declarations ------------------------------
instance Pretty AHsDecl where
	pretty (AHsTypeDecl loc name nameList htype) =
		blankline $
		markLine loc $
		mySep ( [text "type", pretty name]
			++ map pretty nameList
			++ [equals, pretty htype])

	pretty (AHsDataDecl loc context name nameList constrList derives) =
		blankline $
		markLine loc $
		mySep ( [text "data", ppHsContext context, pretty name]
			++ map pretty nameList)
			<+> (myVcat (zipWith (<+>) (equals : repeat (char '|'))
						   (map pretty constrList))
			$$$ ppHsDeriving derives)

	pretty (AHsNewTypeDecl pos context name nameList constr derives) =
		blankline $
		markLine pos $
		mySep ( [text "newtype", ppHsContext context, pretty name]
			++ map pretty nameList)
			<+> equals <+> (pretty constr $$$ ppHsDeriving derives)

	--m{spacing=False}
	-- special case for empty class declaration
	pretty (AHsClassDecl pos context name nameList []) =
		blankline $
		markLine pos $
		mySep ( [text "class", ppHsContext context, pretty name]
			++ map pretty nameList)
	pretty (AHsClassDecl pos context name nameList declList) =
		blankline $
		markLine pos $
		mySep ( [text "class", ppHsContext context, pretty name]
			++ map pretty nameList ++ [text "where"])
		$$$ ppBody classIndent (map pretty declList)

	-- m{spacing=False}
	-- special case for empty instance declaration
	pretty (AHsInstDecl pos context name args []) =
		blankline $
		markLine pos $
		mySep ( [text "instance", ppHsContext context, pretty name]
			++ map ppHsAType args)
	pretty (AHsInstDecl pos context name args declList) =
		blankline $
		markLine pos $
		mySep ( [text "instance", ppHsContext context, pretty name]
			++ map ppHsAType args ++ [text "where"])
		$$$ ppBody classIndent (map pretty declList)

	pretty (AHsDefaultDecl pos htypes) =
		blankline $
		markLine pos $
		text "default" <+> parenList (map pretty htypes)

	pretty (AHsTypeSig pos nameList qualType) =
		blankline $
		markLine pos $
		mySep ((punctuate comma . map pretty $ nameList)
		      ++ [text "::", pretty qualType])

	pretty (AHsFunBind matches) =
		ppBindings (map pretty matches)

	pretty (AHsPatBind pos pat rhs whereDecls) =
		markLine pos $
		myFsep [pretty pat, pretty rhs] $$$ ppWhere whereDecls

	pretty (AHsInfixDecl pos assoc prec opList) =
		blankline $
		markLine pos $
		mySep ([pretty assoc, int prec]
		       ++ (punctuate comma . map pretty $ opList))

instance Pretty AHsAssoc where
	pretty AHsAssocNone  = text "infix"
	pretty AHsAssocLeft  = text "infixl"
	pretty AHsAssocRight = text "infixr"


instance Pretty AHsMatch where
	pretty (AHsMatch pos f ps rhs whereDecls) =
		markLine pos $
		myFsep (lhs ++ [pretty rhs])
		$$$ ppWhere whereDecls
	    where
		lhs = case ps of
			l:r:ps' | isSymbolName f ->
				let hd = [pretty l, pretty f, pretty r] in
				if null ps' then hd
				else parens (myFsep hd) : map (prettyPrec 2) ps'
			_ -> pretty f : map (prettyPrec 2) ps

ppWhere :: [AHsDecl] -> Doc
ppWhere [] = empty
ppWhere l = nest 2 (text "where" $$$ ppBody whereIndent (map pretty l))

------------------------- Data & Newtype Bodies -------------------------
instance Pretty AHsConDecl where
	pretty (AHsRecDecl _pos name fieldList) =
		pretty name <> (braceList . map ppField $ fieldList)

	pretty (AHsConDecl _pos name [l, r]) | isSymbolName name =
		myFsep [prettyPrec prec_btype l, pretty name,
			prettyPrec prec_btype r]
	pretty (AHsConDecl _pos name typeList) =
		mySep $ pretty name : map (prettyPrec prec_atype) typeList

ppField :: ([AHsName],AHsBangType) -> Doc
ppField (names, ty) =
	myFsepSimple $ (punctuate comma . map pretty $ names) ++
		       [text "::", pretty ty]

instance Pretty AHsBangType where
	prettyPrec _ (AHsBangedTy ty) = char '!' <> ppHsAType ty
	prettyPrec p (AHsUnBangedTy ty) = prettyPrec p ty

ppHsDeriving :: [AHsName] -> Doc
ppHsDeriving []  = empty
ppHsDeriving [d] = text "deriving" <+> ppHsQName d
ppHsDeriving ds  = text "deriving" <+> parenList (map ppHsQName ds)

------------------------- Types -------------------------

instance Pretty AHsQualType where
	pretty (AHsQualType context htype) =
		myFsep [ppHsContext context, pretty htype]

ppHsBType :: AHsType -> Doc
ppHsBType = prettyPrec prec_btype

ppHsAType :: AHsType -> Doc
ppHsAType = prettyPrec prec_atype

-- precedences for types
prec_btype, prec_atype :: Int
prec_btype = 1	-- left argument of -> ,
		-- or either argument of an infix data constructor
prec_atype = 2	-- argument of type or data constructor, or of a class

instance Pretty AHsType where
	prettyPrec p (AHsTyFun a b) = parensIf (p > 0) $
		myFsep [ppHsBType a, text "->", pretty b]
	prettyPrec _ (AHsTyTuple l) = parenList . map pretty $ l
	prettyPrec p (AHsTyApp a b)
		| a == alist_tycon = brackets $ pretty b		-- special case
		| otherwise = parensIf (p > prec_btype) $
			myFsep [pretty a, ppHsAType b]
	prettyPrec _ (AHsTyVar name) = pretty name
	prettyPrec _ (AHsTyCon name) = pretty name
	
------------------------- Expressions -------------------------
instance Pretty AHsRhs where
	pretty (AHsUnGuardedRhs e) = equals <+> pretty e
	pretty (AHsGuardedRhss guardList) = myVcat . map pretty $ guardList

instance Pretty AHsGuardedRhs where
	pretty (AHsGuardedRhs _pos guard body) =
		myFsep [char '|', pretty guard, equals, pretty body]

instance Pretty AHsLiteral where
	pretty (AHsInt i)        = integer i
	pretty (AHsChar c)       = text (show c)
	pretty (AHsString s)     = text (show s)
	pretty (AHsFrac r)       = double (fromRational r)
	-- GHC unboxed literals:
	pretty (AHsCharPrim c)   = text (show c)           <> char '#'
	pretty (AHsStringPrim s) = text (show s)           <> char '#'
	pretty (AHsIntPrim i)    = integer i               <> char '#'
	pretty (AHsFloatPrim r)  = float  (fromRational r) <> char '#'
	pretty (AHsDoublePrim r) = double (fromRational r) <> text "##"

instance Pretty AHsExp where
	pretty (AHsLit l) = pretty l
	-- lambda stuff
	pretty (AHsInfixApp a op b) = myFsep [pretty a, pretty op, pretty b]
	pretty (AHsNegApp e) = myFsep [char '-', pretty e]
	pretty (AHsApp a b) = myFsep [pretty a, pretty b]
	pretty (AHsLambda _loc expList body) = myFsep $
		char '\\' : map pretty expList ++ [text "->", pretty body]
	-- keywords
	pretty (AHsLet expList letBody) =
		myFsep [text "let" <+> ppBody letIndent (map pretty expList),
			text "in", pretty letBody]
	pretty (AHsIf cond thenexp elsexp) =
		myFsep [text "if", pretty cond,
			text "then", pretty thenexp,
			text "else", pretty elsexp]
	pretty (AHsCase cond altList) =
		myFsep [text "case", pretty cond, text "of"]
		$$$ ppBody caseIndent (map pretty altList)
	pretty (AHsDo stmtList) =
		text "do" $$$ ppBody doIndent (map pretty stmtList)
	-- Constructors & Vars
	pretty (AHsVar name) = pretty name
	pretty (AHsCon name) = pretty name
	pretty (AHsTuple expList) = parenList . map pretty $ expList
	-- weird stuff
	pretty (AHsParen e) = parens . pretty $ e
	pretty (AHsLeftSection e op) = parens (pretty e <+> pretty op)
	pretty (AHsRightSection op e) = parens (pretty op <+> pretty e)
	pretty (AHsRecConstr c fieldList) =
		pretty c <> (braceList . map pretty $ fieldList)
	pretty (AHsRecUpdate e fieldList) =
		pretty e <> (braceList . map pretty $ fieldList)
	-- patterns
	-- special case that would otherwise be buggy
	pretty (AHsAsPat name (AHsIrrPat e)) =
		myFsep [pretty name <> char '@', char '~' <> pretty e]
	pretty (AHsAsPat name e) = hcat [pretty name, char '@', pretty e]
	pretty AHsWildCard = char '_'
	pretty (AHsIrrPat e) = char '~' <> pretty e
	-- Lists
	pretty (AHsList list) =
		bracketList . punctuate comma . map pretty $ list
	pretty (AHsEnumFrom e) =
		bracketList [pretty e, text ".."]
	pretty (AHsEnumFromTo from to) =
		bracketList [pretty from, text "..", pretty to]
	pretty (AHsEnumFromThen from thenE) =
		bracketList [pretty from <> comma, pretty thenE, text ".."]
	pretty (AHsEnumFromThenTo from thenE to) =
		bracketList [pretty from <> comma, pretty thenE,
			     text "..", pretty to]
	pretty (AHsListComp e stmtList) =
		bracketList ([pretty e, char '|']
			     ++ (punctuate comma . map pretty $ stmtList))
	pretty (AHsExpTypeSig _pos e ty) =
		myFsep [pretty e, text "::", pretty ty]
		
------------------------- Patterns -----------------------------

instance Pretty AHsPat where
	prettyPrec _ (AHsPVar name) = pretty name
	prettyPrec _ (AHsPLit lit) = pretty lit
	prettyPrec _ (AHsPNeg p) = myFsep [char '-', pretty p]
	prettyPrec p (AHsPInfixApp a op b) = parensIf (p > 0) $
		myFsep [pretty a, pretty (AHsQConOp op), pretty b]
	prettyPrec p (AHsPApp n ps) = parensIf (p > 1) $
		myFsep (pretty n : map pretty ps)
	prettyPrec _ (AHsPTuple ps) = parenList . map pretty $ ps
	prettyPrec _ (AHsPList ps) =
		bracketList . punctuate comma . map pretty $ ps
	prettyPrec _ (AHsPParen p) = parens . pretty $ p
	prettyPrec _ (AHsPRec c fields) =
		pretty c <> (braceList . map pretty $ fields)
	-- special case that would otherwise be buggy
	prettyPrec _ (AHsPAsPat name (AHsPIrrPat pat)) =
		myFsep [pretty name <> char '@', char '~' <> pretty pat]
	prettyPrec _ (AHsPAsPat name pat) =
		hcat [pretty name, char '@', pretty pat]
	prettyPrec _ AHsPWildCard = char '_'
	prettyPrec _ (AHsPIrrPat pat) = char '~' <> pretty pat

instance Pretty AHsPatField where
	pretty (AHsPFieldPat name pat) =
		myFsep [pretty name, equals, pretty pat]

------------------------- Case bodies  -------------------------
instance Pretty AHsAlt where
	pretty (AHsAlt _pos e gAlts decls) =
		myFsep [pretty e, pretty gAlts] $$$ ppWhere decls

instance Pretty AHsGuardedAlts where
	pretty (AHsUnGuardedAlt e) = text "->" <+> pretty e
	pretty (AHsGuardedAlts altList) = myVcat . map pretty $ altList

instance Pretty AHsGuardedAlt where
	pretty (AHsGuardedAlt _pos e body) =
		myFsep [char '|', pretty e, text "->", pretty body]
		
------------------------- Statements in monads & list comprehensions -----
instance Pretty AHsStmt where
	pretty (AHsGenerator _loc e from) =
		pretty e <+> text "<-" <+> pretty from
	pretty (AHsQualifier e) = pretty e
	pretty (AHsLetStmt declList) =
		text "let" $$$ ppBody letIndent (map pretty declList)

------------------------- Record updates
instance Pretty AHsFieldUpdate where
	pretty (AHsFieldUpdate name e) =
		myFsep [pretty name, equals, pretty e]
		
------------------------- Names ------------------------- AQUI
instance Pretty AHsOp where
	pretty (AHsQVarOp n) = ppHsQNameInfix n
	pretty (AHsQConOp n) = ppHsQNameInfix n

ppHsQNameInfix :: AHsName -> Doc
ppHsQNameInfix name
	| isSymbolName name = ppHsQName name
	| otherwise = char '`' <> ppHsQName name <> char '`'

instance Pretty AHsName where
	pretty name = parensIf (isSymbolName name) (ppHsQName name)
	
ppHsQName :: AHsName -> Doc
ppHsQName (AUnQual name) = pretty name
ppHsQName (AQual m name) = pretty m <> char '.' <> pretty name
ppHsQName (AHsSpecial s) = pretty s

instance Pretty AHsIdentifier where
    pretty (AHsIdent n) = text n
    pretty (AHsSymbol n) = text n


instance Pretty AHsCName where
    pretty (AHsVarName n) = pretty n
    pretty (AHsConName n) = pretty n	

isSymbolName :: AHsName -> Bool
isSymbolName n = case getName n of
                    Left n -> isSymbolId n
                    Right _ -> False

isSymbolId :: AHsIdentifier -> Bool
isSymbolId (AHsIdent _) = False
isSymbolId _ = True

getName :: AHsName -> Either AHsIdentifier AHsSpecialCon
getName (AQual _ n) = Left n
getName (AUnQual n) = Left n
getName (AHsSpecial s) = Right s

instance Pretty AHsSpecialCon where
    pretty = text  .  specialName

specialName :: AHsSpecialCon -> String
specialName AHsUnitCon = "()"
specialName AHsListCon = "[]"
specialName AHsFunCon = "->"
specialName (AHsTupleCon n) = "(" ++ replicate (n-1) ',' ++ ")"
specialName AHsCons = ":"

ppHsContext :: AHsContext -> Doc
ppHsContext []      = empty
ppHsContext context = mySep [parenList (map ppHsAsst context), text "=>"]

-- hacked for multi-parameter type classes

instance Pretty AHsAsst where
    pretty = ppHsAsst

ppHsAsst :: AHsAsst -> Doc
ppHsAsst (a,ts) = myFsep (ppHsQName a : map ppHsAType ts)
