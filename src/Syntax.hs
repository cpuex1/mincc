module Syntax (UnaryOp (Not, Neg, FNeg)) where

data LiteralKind
    = LUnit
    | LBool {bval :: Bool}
    | LInt {ival :: Int}
    | LFloat {fval :: Float}
    deriving (Show, Eq)

data UnaryOp
    = Not
    | Neg
    | FNeg
    deriving (Show, Eq)

data RelationBinOp = Eq | Le | Ge | Ne | Lt | Gt
data IntBinOp = Add | Sub | Mul | Div
data FloatBinOp = FAdd | FSub | FMul | FDiv
data BinaryOp
    = RelationOp {relop :: RelationBinOp}
    | IntOp {intop :: IntBinOp}
    | FloatOp {floatop :: FloatBinOp}

data ExprKind
    = Const {lit :: LiteralKind}
    | Unary {unop :: UnaryOp, child :: ExprKind}
    | Binary {binop :: BinaryOp, left :: ExprKind, right :: ExprKind}
    | If {cond :: ExprKind, then' :: ExprKind, else' :: ExprKind}
