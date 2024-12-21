{-# LANGUAGE GADTs #-}

module Optim.Pure (definitelyPure) where

import Syntax (Expr (..), KExpr)

definitelyPure :: KExpr -> Bool
definitelyPure (Const{}) = True
definitelyPure (Unary{}) = True
definitelyPure (Binary{}) = True
definitelyPure (If _ _ then' else') = definitelyPure then' && definitelyPure else'
definitelyPure (Let _ _ expr body) = definitelyPure expr && definitelyPure body
definitelyPure (Var{}) = True
definitelyPure (App{}) = False
definitelyPure (Tuple{}) = True
definitelyPure (ArrayCreate{}) = False
definitelyPure (Get{}) =
    -- A global array can be modified in another function.
    False
definitelyPure (Put{}) = False
