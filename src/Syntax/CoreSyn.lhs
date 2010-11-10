This file contains the whole syntax tree of the core language.
Here we don't define anything. We just re-export definitions.

> module Syntax.CoreSyn (module Syntax.NameSyn,
>                        module Syntax.PatSyn,
>                        module Syntax.LitSyn,
>                        module Syntax.TySyn,
>                        module Syntax.ExprSyn,
>                        module Syntax.BindSyn) where

> import Syntax.LitSyn
> import Syntax.NameSyn
> import Syntax.PatSyn
> import Syntax.ExprSyn
> import Syntax.TySyn
> import Syntax.BindSyn
