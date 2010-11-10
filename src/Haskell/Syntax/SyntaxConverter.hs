{-#LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances#-}
{-#LANGUAGE UndecidableInstances                                               #-}

-----------------------------------------------------------------------------
--
-- Module      :  Language.Haskell.Syntax.SyntaxConverter
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Haskell.Syntax.SyntaxConverter (SynConverter, convert)where

import Data.Maybe(isJust, fromJust, maybe)

import Haskell.Syntax.Syntax
import Haskell.Syntax.AnnotatedSyntax

-- a type class to represent the conversion between two specified types

class SynConverter a b | a -> b where
    convert :: a -> b

instance SynConverter a b => SynConverter [a] [b] where
    convert = map convert

instance (SynConverter a c, SynConverter b d) => SynConverter (a,b) (c,d) where
    convert (a,b) = (convert a, convert b)


-- instances to convert the whole syntax tree

instance SynConverter Module AModule where
    convert (Module s) = AModule s

instance SynConverter HsName AHsIdentifier where
    convert (HsIdent s) = AHsIdent s
    convert (HsSymbol s) = AHsSymbol s

instance SynConverter HsSpecialCon AHsSpecialCon where
    convert HsUnitCon = AHsUnitCon
    convert HsListCon = AHsListCon
    convert HsFunCon  = AHsFunCon
    convert HsCons    = AHsCons
    convert (HsTupleCon n) = AHsTupleCon n

instance SynConverter HsQName AHsName where
    convert (Qual m n) = AQual (convert m) (convert n)
    convert (UnQual n) = AUnQual (convert n)
    convert (Special s) = AHsSpecial (convert s)

instance SynConverter HsQOp AHsOp where
    convert (HsQVarOp n) = AHsQVarOp (convert n)
    convert (HsQConOp n) = AHsQConOp (convert n)

instance SynConverter HsCName AHsCName where
    convert (HsVarName n) = AHsVarName (hsNameToAHsName n)
    convert (HsConName n) = AHsConName (hsNameToAHsName n)

instance SynConverter HsModule AHsModule where
    convert (HsModule loc mod exps imps decls) = AHsModule loc (convert mod)
                                                               exps'
                                                               (convert imps)
                                                               (convert decls)
                                                 where
                                                    exps' = maybe Nothing (Just  .  convert) exps

instance SynConverter HsExportSpec AHsExportSpec where
    convert (HsEVar n) = AHsEVar (convert n)
    convert (HsEAbs n) = AHsEAbs (convert n)
    convert (HsEThingAll n) = AHsEThingAll (convert n)
    convert (HsEThingWith n ns) = AHsEThingWith (hsQNameToAHsName n)
                                                (convert ns)
    convert (HsEModuleContents m) = AHsEModuleContents (convert m)

instance SynConverter HsImportDecl AHsImportDecl where
    convert (HsImportDecl loc m qual impas impspec) = AHsImportDecl loc
                                                                    m'
                                                                    qual
                                                                    impas'
                                                                    impspec'
                                                      where
                                                        m' = convert m
                                                        impas' = maybe Nothing
                                                                       (Just  .  convert)
                                                                       impas
                                                        impspec' = maybe Nothing
                                                                         convert'
                                                                         impspec

                                                        convert' (b, specs) = Just (b, convert specs)

instance SynConverter HsImportSpec AHsImportSpec where
    convert (HsIVar n) = AHsIVar (hsNameToAHsName n)
    convert (HsIAbs n) = AHsIAbs (hsNameToAHsName n)
    convert (HsIThingAll n) = AHsIThingAll (hsNameToAHsName n)
    convert (HsIThingWith n ncs) = AHsIThingWith (hsNameToAHsName n)
                                                 (convert ncs)

instance SynConverter HsAssoc AHsAssoc where
    convert HsAssocNone = AHsAssocNone
    convert HsAssocLeft = AHsAssocLeft
    convert HsAssocRight = AHsAssocRight

instance SynConverter HsDecl AHsDecl where
    convert (HsTypeDecl loc n ns ty) = AHsTypeDecl loc
                                                   (hsNameToAHsName n)
                                                   (map hsNameToAHsName ns)
                                                   (convert ty)
    convert (HsDataDecl loc ctx n ns cons qns) = AHsDataDecl loc
                                                             (convert ctx)
                                                             (hsNameToAHsName n)
                                                             (map hsNameToAHsName ns)
                                                             (convert cons)
                                                             (convert qns)
    convert (HsInfixDecl loc ass n ops) = AHsInfixDecl loc
                                                       (convert ass)
                                                       n
                                                       (map hsOpToAHsOp ops)
    convert (HsNewTypeDecl loc ctx n ns con qns) = AHsNewTypeDecl loc
                                                                  (convert ctx)
                                                                  (hsNameToAHsName n)
                                                                  (map hsNameToAHsName ns)
                                                                  (convert con)
                                                                  (convert qns)
    convert (HsClassDecl loc ctx n ns decls) = AHsClassDecl loc
                                                            (convert ctx)
                                                            (hsNameToAHsName n)
                                                            (map hsNameToAHsName ns)
                                                            (convert decls)
    convert (HsInstDecl loc ctx n ns decls) = AHsInstDecl loc
                                                          (convert ctx)
                                                          (convert n)
                                                          (convert ns)
                                                          (convert decls)
    convert (HsDefaultDecl loc tys) = AHsDefaultDecl loc (convert tys)
    convert (HsTypeSig loc ns qty) = AHsTypeSig loc
                                                (map hsNameToAHsName ns)
                                                (convert qty)
    convert (HsFunBind ms) = AHsFunBind (convert ms)
    convert (HsPatBind loc p rhs decls) = AHsPatBind loc
                                                     (convert p)
                                                     (convert rhs)
                                                     (convert decls)

instance SynConverter HsMatch AHsMatch where
    convert (HsMatch loc n ps rhs decls) = AHsMatch loc
                                                    (hsNameToAHsName n)
                                                    (convert ps)
                                                    (convert rhs)
                                                    (convert decls)

instance SynConverter HsConDecl AHsConDecl where
    convert (HsConDecl loc n tys) = AHsConDecl loc (hsNameToAHsName n)
                                                   (convert tys)
    convert (HsRecDecl loc n fs) = AHsRecDecl loc (hsNameToAHsName n)
                                                  (map convert' fs)
                                   where
                                      convert' (ns, ty) = (map hsNameToAHsName ns,
                                                           convert ty)

instance SynConverter HsBangType AHsBangType where
    convert (HsBangedTy ty) = AHsBangedTy (convert ty)
    convert (HsUnBangedTy ty) = AHsUnBangedTy (convert ty)

instance SynConverter HsRhs AHsRhs where
    convert (HsUnGuardedRhs e) = AHsUnGuardedRhs (convert e)
    convert (HsGuardedRhss gs) = AHsGuardedRhss (convert gs)

instance SynConverter HsGuardedRhs AHsGuardedRhs where
    convert (HsGuardedRhs loc g e) = AHsGuardedRhs loc
                                                   (convert g)
                                                   (convert e)

instance SynConverter HsQualType AHsQualType where
    convert (HsQualType ctx ty) = AHsQualType (convert ctx) (convert ty)

instance SynConverter HsType AHsType where
    convert (HsTyFun t1 t2) = AHsTyFun (convert t1) (convert t2)
    convert (HsTyTuple ts) = AHsTyTuple (convert ts)
    convert (HsTyApp t1 t2) = AHsTyApp (convert t1) (convert t2)
    convert (HsTyAnd t1 ) = AHsTyAnd (map convert t1)
    convert (HsTyVar n) = AHsTyVar (hsNameToAHsName n)
    convert (HsTyCon qn) = AHsTyCon (convert qn)

instance SynConverter HsLiteral AHsLiteral where
    convert (HsChar c) = AHsChar c
    convert (HsString s) = AHsString s
    convert (HsInt i) = AHsInt i
    convert (HsFrac r) = AHsFrac r
    convert _ = error "Not supported feature: Unboxed types!"

instance SynConverter HsExp AHsExp where
    convert (HsVar qn) = AHsVar (convert qn)
    convert (HsCon qn) = AHsCon (convert qn)
    convert (HsLit lit) = AHsLit (convert lit)
    convert (HsInfixApp e1 qop e2) = AHsInfixApp (convert e1)
                                                 (convert qop)
                                                 (convert e2)
    convert (HsApp e1 e2) = AHsApp (convert e1)
                                   (convert e2)
    convert (HsNegApp e) = AHsNegApp (convert e)
    convert (HsLambda loc ps e) = AHsLambda loc (convert ps)
                                                (convert e)
    convert (HsLet decls e) = AHsLet (convert decls) (convert e)
    convert (HsIf e1 e2 e3) = AHsIf (convert e1)
                                    (convert e2)
                                    (convert e3)
    convert (HsCase e alts) = AHsCase (convert e)
                                      (convert alts)
    convert (HsDo stmts) = AHsDo (convert stmts)
    convert (HsTuple exps) = AHsTuple (convert exps)
    convert (HsList exps) = AHsList (convert exps)
    convert (HsParen e) = AHsParen (convert e)
    convert (HsLeftSection e qop) = AHsLeftSection (convert e)
                                                   (convert qop)
    convert (HsRightSection qop e) = AHsRightSection (convert qop)
                                                     (convert e)
    convert (HsRecConstr qn upds) = AHsRecConstr (convert qn)
                                                 (convert upds)
    convert (HsRecUpdate e upds) = AHsRecUpdate (convert e)
                                                (convert upds)
    convert (HsEnumFrom e) = AHsEnumFrom (convert e)
    convert (HsEnumFromTo e1 e2) = AHsEnumFromTo (convert e1)
                                                 (convert e2)
    convert (HsEnumFromThen e1 e2) = AHsEnumFromThen (convert e1)
                                                     (convert e2)
    convert (HsEnumFromThenTo e1 e2 e3) = AHsEnumFromThenTo (convert e1)
                                                            (convert e2)
                                                            (convert e3)
    convert (HsListComp e stmts) = AHsListComp (convert e)
                                               (convert stmts)
    convert (HsExpTypeSig loc e qty) = AHsExpTypeSig loc
                                                     (convert e)
                                                     (convert qty)

instance SynConverter HsPat AHsPat where
    convert (HsPVar n) = AHsPVar (hsNameToAHsName n)
    convert (HsPLit lit) = AHsPLit (convert lit)
    convert (HsPNeg p) = AHsPNeg (convert p)
    convert (HsPInfixApp p1 qn p2) = AHsPInfixApp (convert p1)
                                                  (convert qn)
                                                  (convert p2)
    convert (HsPApp qn ps) = AHsPApp (convert qn)
                                     (convert ps)
    convert (HsPTuple ps) = AHsPTuple (convert ps)
    convert (HsPList ps) = AHsPList (convert ps)
    convert (HsPParen p) = AHsPParen (convert p)
    convert (HsPRec qn ps) = AHsPRec (convert qn)
                                     (convert ps)
    convert (HsPAsPat n p) = AHsPAsPat (hsNameToAHsName n)
                                       (convert p)
    convert HsPWildCard = AHsPWildCard
    convert (HsPIrrPat p) = AHsPIrrPat (convert p)

instance SynConverter HsPatField AHsPatField where
    convert (HsPFieldPat qn p) = AHsPFieldPat (convert qn)
                                              (convert p)

instance SynConverter HsStmt AHsStmt where
    convert (HsGenerator loc p e) = AHsGenerator loc
                                                 (convert p)
                                                 (convert e)
    convert (HsQualifier e) = AHsQualifier (convert e)
    convert (HsLetStmt decls) = AHsLetStmt (convert decls)

instance SynConverter HsFieldUpdate AHsFieldUpdate where
    convert (HsFieldUpdate qn e) = AHsFieldUpdate (convert qn)
                                                  (convert e)

instance SynConverter HsAlt AHsAlt where
    convert (HsAlt loc p galts decls) = AHsAlt loc
                                               (convert p)
                                               (convert galts)
                                               (convert decls)

instance SynConverter HsGuardedAlts AHsGuardedAlts where
    convert (HsUnGuardedAlt e) = AHsUnGuardedAlt (convert e)
    convert (HsGuardedAlts galts) = AHsGuardedAlts (convert galts)

instance SynConverter HsGuardedAlt AHsGuardedAlt where
    convert (HsGuardedAlt loc e1 e2) = AHsGuardedAlt loc
                                                     (convert e1)
                                                     (convert e2)

hsNameToAHsName :: HsName -> AHsName
hsNameToAHsName = AUnQual  .  convert

hsOpToAHsOp :: HsOp -> AHsOp
hsOpToAHsOp (HsVarOp n) = AHsQConOp (hsNameToAHsName n)
hsOpToAHsOp (HsConOp n) = AHsQConOp (hsNameToAHsName n)

hsQNameToAHsName (Qual m n) = AQual (convert m) (convert n)
hsQNameToAHsName (UnQual n) = AUnQual (convert n)
hsQNameToAHsName (Special s) = AHsSpecial (convert s)

