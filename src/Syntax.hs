module Syntax (
    Literal,
    LiteralKind (LUnit, LBool, LInt, LFloat),
    UnaryOp (Not, Neg, FNeg),
    RelationBinOp (Eq, Le, Ge, Ne, Lt, Gt),
    IntBinOp (Add, Sub, Mul, Div),
    FloatBinOp (FAdd, FSub, FMul, FDiv),
    BinaryOp (RelationOp, IntOp, FloatOp),
    Ident,
    Pattern (PVar, PRec, PTuple),
    LetBinder (LetBinder),
    Expr,
    ExprKind (Const, Unary, Binary, If, Let, Then, Var, App, Tuple, ArrayMake, Get, Set),
) where

import Data.Text (Text)
import Text.Megaparsec.Pos (SourcePos)

type Literal = (SourcePos, LiteralKind)

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

data RelationBinOp = Eq | Le | Ge | Ne | Lt | Gt deriving (Show, Eq)
data IntBinOp = Add | Sub | Mul | Div deriving (Show, Eq)
data FloatBinOp = FAdd | FSub | FMul | FDiv deriving (Show, Eq)
data BinaryOp
    = RelationOp {relop :: RelationBinOp}
    | IntOp {intop :: IntBinOp}
    | FloatOp {floatop :: FloatBinOp}
    deriving (Show, Eq)

type Ident = (SourcePos, IdentKind)
type IdentKind = Text

data Pattern
    = PVar Ident
    | PRec Ident [Ident]
    | PTuple [Ident]
    deriving (Show, Eq)

data LetBinder = LetBinder {pat :: Pattern, value :: Expr} deriving (Show, Eq)

type Expr = (SourcePos, ExprKind)

data ExprKind
    = Const {lit :: Literal}
    | Unary {unop :: UnaryOp, child :: Expr}
    | Binary {binop :: BinaryOp, left :: Expr, right :: Expr}
    | If {cond :: Expr, then' :: Expr, else' :: Expr}
    | Let {binder :: LetBinder, body :: Expr}
    | Then {expr1 :: Expr, expr2 :: Expr}
    | Var {ident :: Ident}
    | App {expr1 :: Expr, exprs :: [Expr]}
    | Tuple {exprs :: [Expr]}
    | ArrayMake {expr1 :: Expr, expr2 :: Expr}
    | Get {expr1 :: Expr, expr2 :: Expr}
    | Set {expr1 :: Expr, expr2 :: Expr}
    deriving (Show, Eq)
