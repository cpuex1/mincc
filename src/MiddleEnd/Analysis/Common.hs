{-# LANGUAGE GADTs #-}

module MiddleEnd.Analysis.Common (
    definitelyPure,
    exprSize,
    isUsed,
) where

import Control.Monad.State (State, execState, modify)
import Syntax (Expr (..), Ident, KExpr, visitExprM)

{- | Checks if the expression is pure.
This analysis is too conservative and can return false negatives.
-}
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

-- | Calculates the size of the expression.
exprSize :: KExpr -> Int
exprSize (Const{}) = 1
exprSize (Unary{}) = 1
exprSize (Binary{}) = 1
exprSize (If _ _ then' else') = 1 + exprSize then' + exprSize else'
exprSize (Let _ _ expr body) = 1 + exprSize expr + exprSize body
exprSize (Var _ _) = 1
exprSize (App _ _ args) = 1 + length args
exprSize (Tuple _ args) = 1 + length args
exprSize (ArrayCreate{}) = 1
exprSize (Get{}) = 1
exprSize (Put{}) = 1

-- | Checks if the identifier is used in the expression.
isUsed :: Ident -> KExpr -> Bool
isUsed ident expr = execState isUsed' False
  where
    tryMarkUsed :: Ident -> State Bool ()
    tryMarkUsed ident' = modify (\used -> used || ident' == ident)

    isUsed' :: State Bool KExpr
    isUsed' = do
        visitExprM
            pure
            (\i -> tryMarkUsed i >> pure i)
            (\i -> tryMarkUsed i >> pure i)
            expr
