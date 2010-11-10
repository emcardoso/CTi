In this module we define the desugar process of expressions.

> module Desugar.DesugarExpr where

> import Control.Monad
> import Haskell.Syntax.Syntax hiding (Qual, UnQual)
> import Haskell.Pretty.Pretty (prettyPrint)

> import Syntax.CoreSyn

> import Desugar.Desugar
> import Desugar.DesugarLit
> import Desugar.DesugarName
> import Desugar.DesugarDecl
> import Desugar.DesugarPat
> import Desugar.DesugarTy

> import Prelude.PreludeBuiltIn


The definition of the desugar of expressions. 

> desugarHsExp :: HsExp -> DesugarM (Expr Name)
> desugarHsExp (HsVar qn)                  = liftM Var (desugarHsQName qn)
> desugarHsExp (HsCon qn)                  = liftM Con (desugarHsQName qn)
> desugarHsExp (HsLit li)                  = desugarLit li
> desugarHsExp (HsInfixApp e1 op e2)       = desugarInfixApp e1 op e2
> desugarHsExp (HsApp e1 e2)               = liftM2 App (desugarHsExp e1) (desugarHsExp e2)
> desugarHsExp (HsNegApp e)                = liftM (App (Var negName)) (desugarHsExp e)
> desugarHsExp (HsLambda _ ps e)           = liftM2 Lam (mapM desugarHsPat ps) (desugarHsExp e)
> desugarHsExp (HsLet ds e)                = liftM2 Let (mapM desugarHsDecl ds) (desugarHsExp e)
> desugarHsExp (HsIf e1 e2 e3)             = liftM3 If (desugarHsExp e1) (desugarHsExp e2) (desugarHsExp e3)
> desugarHsExp (HsCase e as)               = liftM2 Case (desugarHsExp e) (mapM desugarHsAlt as)
> desugarHsExp (HsDo stmts)                = desugarStmts stmts
> desugarHsExp (HsTuple es)                = liftM Tuple (mapM desugarHsExp es)
> desugarHsExp (HsList es)                 = liftM List (mapM desugarHsExp es)
> desugarHsExp (HsParen e)                 = desugarHsExp e
> desugarHsExp (HsLeftSection e op)        = desugarSection True e op
> desugarHsExp (HsRightSection op e)       = desugarSection False e op
> desugarHsExp (HsRecConstr _ _ )          = throwError "Records aren't supported. Please use conventional data types."
> desugarHsExp (HsRecUpdate _ _ )          = throwError "Records aren't supported. Please use conventional data types."
> desugarHsExp (HsEnumFrom e)              = desugarEnum enumFromSyn [e]
> desugarHsExp (HsEnumFromThen e1 e2)      = desugarEnum enumFromThenSyn [e1, e2]
> desugarHsExp (HsEnumFromTo e1 e2)        = desugarEnum enumFromToSyn [e1, e2]
> desugarHsExp (HsEnumFromThenTo e1 e2 e3) = desugarEnum enumFromThenToSyn [e1, e2, e3]
> desugarHsExp (HsListComp e stmts)        = desugarListComp e stmts
> desugarHsExp (HsExpTypeSig _ e qt)       = desugarExpTySig e qt

> desugarHsAlt :: HsAlt -> DesugarM (Alt Name)
> desugarHsAlt (HsAlt _ p gs decls) = do
>                                       p'     <- desugarHsPat p
>                                       gs'    <- desugarHsGuardedAlts gs
>                                       decls' <- mapM desugarHsDecl decls
>                                       return $ case decls' of
>                                                    [] -> ([p'], gs') 
>                                                    ds -> ([p'], Let decls' gs')

> desugarHsGuardedAlts :: HsGuardedAlts -> DesugarM (Expr Name)
> desugarHsGuardedAlts (HsUnGuardedAlt e) = desugarHsExp e
> desugarHsGuardedAlts (HsGuardedAlts as) = mapM desugarHsGuardedAlt as >>= \as' -> return (result as')
>                                            where 
>                                               result = foldr ifthenelse errPatFail
>                                               ifthenelse (g,e) ac = If g e ac
>                                               

> desugarHsGuardedAlt :: HsGuardedAlt -> DesugarM (Expr Name, Expr Name)
> desugarHsGuardedAlt (HsGuardedAlt _ e1 e2) = liftM2 ((,)) (desugarHsExp e1) (desugarHsExp e2)

This function is reponsible for take care of numeric
literals that must be desugared correctly.

> desugarLit :: HsLiteral -> DesugarM (Expr Name)
> desugarLit (HsChar c)   = return (Const (LitChar c))
> desugarLit (HsString s) = return (Const (LitStr s))
> desugarLit l@(HsInt i)  = desugarNumLit fromIntegerSyn l
> desugarLit l@(HsFrac r) = desugarNumLit fromRationalSyn l
> desugarLit l            = throwError ("Unboxed literals aren't supported!\n" ++ (prettyPrint l))

> desugarNumLit :: Expr Name -> HsLiteral -> DesugarM (Expr Name)
> desugarNumLit n l = desugarHsLiteral l >>= \l' -> return (App n (Const l'))

This function do the desugar of do-expressions
        
> desugarStmts :: [HsStmt] -> DesugarM (Expr Name)
> desugarStmts [s]    = desugarStmt s
> desugarStmts ((HsQualifier e):ss) = do
>                                         ap1 <- liftM (App thenMonadSyn) (desugarHsExp e)
>                                         ap2 <- desugarStmts ss
>                                         return (App ap1 ap2)
> desugarStmts ((HsGenerator _ p e):ss) = do
>                                           e'  <- desugarHsExp e
>                                           p'  <- desugarHsPat p
>                                           ss' <- desugarStmts ss
>                                           return (mkBindM e' p' ss')
> desugarStmts ((HsLetStmt decls) : ss) = do 
>                                           decls' <- mapM desugarHsDecl decls
>                                           ss' <- desugarStmts ss
>                                           return (Let decls' ss')

> desugarStmt :: HsStmt -> DesugarM (Expr Name)                                          
> desugarStmt (HsQualifier e) = liftM (App thenMonadSyn) (desugarHsExp e)
> desugarStmt (HsGenerator _ p e) = do
>                                     p' <- desugarHsPat p
>                                     e' <- desugarHsExp e
>                                     return (mkBindM e' p' errPatFail)
> desugarStmt (HsLetStmt _) = throwError "The last statement in a do-expression must be an expression"

> mkBindM :: Expr Name -> Pat Name -> Expr Name -> Expr Name
> mkBindM e' p' ss' = App (App bindMonadSyn e') (Lam [p'] ss')

> mkExpr :: Name -> Expr Name -> Expr Name
> mkExpr n e = App (App bindMonadSyn e) (Var n)

This function do the desugar of infix applications

> desugarInfixApp :: HsExp -> HsQOp -> HsExp -> DesugarM (Expr Name)
> desugarInfixApp e1 op e2 = do
>                               e1' <- desugarHsExp e1
>                               op' <- desugarHsQOp op
>                               e2' <- desugarHsExp e2
>                               return (App (App (Var op') e1') e2')


This function do the desugar of sections. True = Left section.

> desugarSection :: Bool -> HsExp -> HsQOp -> DesugarM (Expr Name)
> desugarSection b e op = do
>                           x   <- newVar
>                           e'  <- desugarHsExp e
>                           op' <- desugarHsQOp op
>                           let l     = Lam [PVar x]
>                               f a b = App (App (Var op') a) b
>                               x'    = Var x
>                           return $ if b then l (f e' x')
>                                       else l (f x' e')

This function does the desugar of the list enumerations

> desugarEnum :: Expr Name -> [HsExp] -> DesugarM (Expr Name)
> desugarEnum e es = do
>                       es' <- mapM desugarHsExp es
>                       return (foldl App e es')

This function does the desugar of the list comprehensions

> desugarListComp :: HsExp -> [HsStmt] -> DesugarM (Expr Name)
> desugarListComp e [] = liftM (List . (:[])) (desugarHsExp e)
> desugarListComp e ((HsQualifier e1):ss)     = do
>                                                 e1' <- desugarHsExp e1
>                                                 el  <- desugarListComp e ss
>                                                 return (If e1' el listEmptySyn)
> desugarListComp e ((HsGenerator _ p e1):ss) = do
>                                                 p'  <- desugarHsPat p
>                                                 e1' <- desugarHsExp e1
>                                                 el  <- desugarListComp e ss
>                                                 v   <- newVar
>                                                 let
>                                                   v' = Var v
>                                                   bs = mkListBind v p' el
>                                                   ex = App (App concatMapSyn v') e1'
>                                                 return (Let bs ex)
> desugarListComp e ((HsLetStmt decls):ss)    = do
>                                                 decls' <- mapM desugarHsDecl decls
>                                                 el     <- desugarListComp e ss
>                                                 return (Let decls' el)

> mkListBind v p el = [(FunBind v Nothing (Just [([p], el)])), 
>                      (FunBind v Nothing (Just [([PWildCard], listEmptySyn)]))]

This function does the desugaring of expression type signatures

> desugarExpTySig :: HsExp -> HsQualType -> DesugarM (Expr Name)
> desugarExpTySig e qt = do
>                           e'  <- desugarHsExp e
>                           qt' <- desugarHsQualType qt
>                           v   <- newVar
>                           let
>                             bs = [(FunBind v (Just qt') Nothing), (FunBind v Nothing (Just [([], e')]))]
>                           return (Let bs (Var v))

> errPatFail :: Expr Name
> errPatFail = App errorSyn (Const (LitStr "Unmatched pattern!"))

