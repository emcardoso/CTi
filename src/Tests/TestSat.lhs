> module Tests.TestSat where

Just a driver to test the sat algorithm

> import Control.Monad
> import Control.Monad.Error
> import Tc.TcSat
> import Tc.TcMonad
> import Tc.Class
> import Syntax.CoreSyn
> import Utils.Env

> kappa1 :: [Pred Name]
> kappa1 = [(x, [a]), (y, [b])]
>          where
>              x = Unqual "X"
>              y = Unqual "Y"
>              a = TVar (Free 0 Star)
>              b = TVar (Free 0 Star)

> kappa2 :: [Pred Name]
> kappa2 = [(x, [a])]
>          where
>              x = Unqual "X"
>              a = TVar (Free 0 Star)


> env :: TcEnv
> env = TcEnv clsenv empty 0 []
>       where 
>           clsenv = fromList [(xn, x), (yn,y)]
>           xn = (Unqual "X")
>           x = Class xn a [] [] [xint]
>           xint = Inst xn [TCon (TyCon (Unqual "Int") Star)] []
>           xbool = Inst xn [TCon (TyCon (Unqual "Bool") Star)] []
>           a = [(Free 0 Star)] 
>           yn = (Unqual "Y")
>           y = Class yn a [] [] [yfloat, ychar ]
>           yfloat = Inst yn [TCon (TyCon (Unqual "Float") Star)] []
>           ychar = Inst yn [TCon (TyCon (Unqual "Char") Star)] []


> test k = do 
>           (r,e) <- runTcM (sat k) env
>           either error print r
>           print (subst e)

