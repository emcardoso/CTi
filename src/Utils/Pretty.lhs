Definition of a type class for pretty pritting the desugared syntax

> module Utils.Pretty(Pretty(..), pat, pdot, plam, pcase, pof, pUnwords,
>                     pUnlines, pPunctuate, ident, plet, pin, pif, pthen,
>                     pelse, peq, parrow, pdarrow,
>                     module Text.PrettyPrint.HughesPJ) where


> import Text.PrettyPrint.HughesPJ

> class Pretty a where
>   pprint :: a -> Doc

> pPunctuate :: Pretty a => Doc -> [a] -> Doc
> pPunctuate d = foldr (commaCons . pprint) empty
>                where
>                   commaCons x y
>                           | (show y) == [] = x 
>                           | otherwise = x <> comma <+> y

> pUnwords :: Pretty a => [a] -> Doc
> pUnwords = foldr ((<+>) . pprint) empty

> pUnlines :: Pretty a => [a] -> Doc
> pUnlines = foldr (($+$) . pprint) empty


some auxiliar sintatic elements

> peq :: Doc
> peq = char '='

> plet :: Doc
> plet = text "let"

> pin :: Doc
> pin = text "in"

> pif :: Doc
> pif = text "if"

> pthen :: Doc
> pthen = text "then"

> pelse :: Doc
> pelse = text "else"

> pat :: Doc 
> pat = char '@'

> plam :: Doc
> plam = char '\\'

> pdot  :: Doc
> pdot = char '.'

> pcase :: Doc
> pcase = text "case"

> pof :: Doc
> pof = text "of"

> parrow :: Doc
> parrow = text "->"

> pdarrow :: Doc
> pdarrow = text "=>"

A constant for represent identation

> ident :: Int
> ident = 4
