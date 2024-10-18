module Syntax (Literal, LiteralKind (LUnit, LBool, LInt, LFloat), UnaryOp (Not, Neg, FNeg), Ident) where
import Text.Parsec.Pos (SourcePos)
import Data.Text (Text)

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

data RelationBinOp = Eq | Le | Ge | Ne | Lt | Gt
data IntBinOp = Add | Sub | Mul | Div
data FloatBinOp = FAdd | FSub | FMul | FDiv
data BinaryOp
    = RelationOp {relop :: RelationBinOp}
    | IntOp {intop :: IntBinOp}
    | FloatOp {floatop :: FloatBinOp}

type Ident = (SourcePos, IdentKind)
type IdentKind = Text

data Pattern = PVar { var :: Ident, args :: [Ident] } | PTuple [Ident]

data LetBinder = LetBinder {pat :: Pattern, value :: Expr}

type Expr = (SourcePos, ExprKind)

data ExprKind
    = Const {lit :: Literal}
    | Unary {unop :: UnaryOp, child :: Expr}
    | Binary {binop :: BinaryOp, left :: Expr, right :: Expr}
    | If {cond :: Expr, then' :: Expr, else' :: Expr}
    | Let { binder :: LetBinder, body :: Expr }
    | Then { expr1 :: Expr, expr2 :: Expr }
    | Var { ident :: Ident }
    | App { expr1 :: Expr, expr2 :: Expr }
    | Tuple { exprs :: [Expr] }
    | ArrayMake { expr1 :: Expr, expr2 :: Expr }
    | Get { expr1 :: Expr, expr2 :: Expr }
    | Set { expr1 :: Expr, expr2 :: Expr }
