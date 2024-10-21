module Syntax (
    Literal (LUnit, LBool, LInt, LFloat),
    UnaryOp (Not, Neg, FNeg),
    RelationBinOp (Eq, Le, Ge, Ne, Lt, Gt),
    IntBinOp (Add, Sub, Mul, Div),
    FloatBinOp (FAdd, FSub, FMul, FDiv),
    BinaryOp (RelationOp, IntOp, FloatOp),
    Ident,
    Pattern (PVar, PRec, PTuple),
    LetBinder (LetBinder),
    Expr,
    ExprKind (Const, Unary, Binary, If, Let, Then, Var, App, Tuple, ArrayCreate, Get, Set),
) where

import Data.Text (Text)
import Text.Megaparsec.Pos (SourcePos)

data Literal
    = LUnit
    | LBool Bool
    | LInt Int
    | LFloat Float
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
    = RelationOp RelationBinOp
    | IntOp IntBinOp
    | FloatOp FloatBinOp
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
    = Const Literal
    | Unary UnaryOp Expr
    | Binary BinaryOp Expr Expr
    | If Expr Expr Expr
    | Let LetBinder Expr
    | Then Expr Expr
    | Var Ident
    | App Expr [Expr]
    | Tuple [Expr]
    | ArrayCreate Expr Expr
    | Get Expr Expr
    | Set Expr Expr
    deriving (Show, Eq)
