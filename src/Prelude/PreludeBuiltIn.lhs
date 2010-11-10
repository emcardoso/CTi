This file contains the types and auxiliar definitions in the 
prelude.

> module Prelude.PreludeBuiltIn where

> import Syntax.CoreSyn
> import Tc.Assumption

Here some definitions of basic types of the language

> kindtable = [int, bool, char, float, list, unit, arrow, io]

> builtincons = [unitcon, conscon, listcon]
> builtinfuns = [seqprim]

> int = TyCon (Unqual "Int") Star
> integer = TyCon (Unqual "Integer") Star
> bool = TyCon (Unqual "Bool") Star
> char = TyCon (Unqual "Char") Star
> float = TyCon (Unqual "Float") Star
> ratio = TyCon (Unqual "Ratio") (KFun Star Star)
> list  = TyCon ListCon (KFun Star Star)
> unit  = TyCon (TupleCon 0) Star
> arrow = TyCon ArrowCon (KFun Star (KFun Star Star))
> io = TyCon (Unqual "IO") (KFun Star Star)

> tInteger = TCon integer
> tInt = TCon int
> tBool = TCon bool
> tChar = TCon char
> tFloat = TCon float
> tRatio t = TyApp (TCon ratio) t
> tRational = tRatio tInteger
> tList t = TyApp (TCon list) t
> tString = TyList tChar
> tTuple ts = TyTuple ts
> tIO t = TyApp (TCon io) t

> (+->) :: Ty Name -> Ty Name -> Ty Name
> x +-> y = TyFun x y 

> av = TVar (Bound (Unqual "a") Star)
> bvar = TVar (Bound (Unqual "b") Star)


> unitcon = (TupleCon 0) :>: (Forall ([] :=> TCon unit))
> listcon = ListCon :>: (Forall ([] :=> tList av))
> conscon = ConsCon :>: (Forall ([] :=> TyFun av (TyFun (tList av) (tList av))))
> seqprim = (Unqual "seq") :>: (Forall ([] :=> TyFun av (TyFun bvar bvar)))

> primint2char = (mkPrimitiveName "primIntToChar") :>: (Forall ([] :=> (tInt +-> tChar)))
> primchar2int = (mkPrimitiveName "primCharToInt") :>: (Forall ([] :=> (tChar +-> tInt)))
> primunicodemaxchar = (mkPrimitiveName "primUnicodeMaxChar") :>: (Forall ([] :=> tChar))
> primbindio = (mkPrimitiveName "bindIO") :>: (Forall ([] :=> ((tIO av) +-> (av +-> (tIO bvar)) +-> (tIO bvar))))
> primreturnio = (mkPrimitiveName "returnIO") :>: (Forall ([] :=> (av +-> (tIO av))))
> primioerror = (mkPrimitiveName "ioError") :>: (Forall ([] :=> (tString +-> (tIO av))))
> primeqint = (mkPrimitiveName "primEqInt") :>: (Forall ([] :=> (tInt +-> (tInt +-> tBool))))
> primlteint = (mkPrimitiveName "primLTEInt") :>: (Forall ([] :=> (tInt +-> (tInt +-> tBool))))
> primplusint = (mkPrimitiveName "primPlusInt") :>: (Forall ([] :=> (tInt +-> (tInt +-> tInt))))
> primminusint = (mkPrimitiveName "primMinusInt") :>: (Forall ([] :=> (tInt +-> (tInt +-> tInt))))
> primmultint = (mkPrimitiveName "primMultInt") :>: (Forall ([] :=> (tInt +-> (tInt +-> tInt))))
> primintegertoint = (mkPrimitiveName "primIntegerToInt") :>: (Forall ([] :=> (tInteger +-> tInt)))

> primeqinteger = (mkPrimitiveName "primEqInteger") :>: (Forall ([] :=> (tInteger +-> (tInteger +-> tBool))))
> primlteinteger = (mkPrimitiveName "primLTEInteger") :>: (Forall ([] :=> (tInteger +-> (tInteger +-> tBool))))
> primplusinteger = (mkPrimitiveName "primPlusInteger") :>: (Forall ([] :=> (tInteger +-> (tInteger +-> tInteger))))
> primminusinteger = (mkPrimitiveName "primMinusInteger") :>: (Forall ([] :=> (tInteger +-> (tInteger +-> tInteger))))
> primmultinteger = (mkPrimitiveName "primMultInteger") :>: (Forall ([] :=> (tInteger +-> (tInteger +-> tInteger))))



Some names used by desugaring process

> enumFromSyn :: Expr Name 
> enumFromSyn = mkPreludeVar "enumFrom"

> enumFromThenSyn :: Expr Name 
> enumFromThenSyn = mkPreludeVar "enumFromThen"

> enumFromToSyn :: Expr Name 
> enumFromToSyn = mkPreludeVar "enumFromTo"

> enumFromThenToSyn :: Expr Name 
> enumFromThenToSyn = mkPreludeVar "enumFromThenTo"

> fromIntegerSyn :: Expr Name
> fromIntegerSyn = mkPreludeVar "fromInteger"

> fromRationalSyn :: Expr Name
> fromRationalSyn =  mkPreludeVar "fromRational"

> thenMonadSyn :: Expr Name
> thenMonadSyn = mkPreludeVar ">>"

> bindMonadSyn :: Expr Name
> bindMonadSyn = mkPreludeVar ">>="

> errorSyn :: Expr Name
> errorSyn = mkPreludeVar "error"

> listEmptySyn :: Expr Name
> listEmptySyn = List []

> concatMapSyn :: Expr Name
> concatMapSyn = mkPreludeVar "concatMap"

Names provided by the prelude

> negName :: Name
> negName = mkPreludeName "negate"

Some auxiliar functions

> mkPreludeVar :: String -> Expr Name
> mkPreludeVar = Var . mkPreludeName

> mkPreludeName :: String -> Name
> mkPreludeName = Unqual -- Qual "Prelude"

> mkPrimitiveName :: String -> Name 
> mkPrimitiveName = Unqual 
