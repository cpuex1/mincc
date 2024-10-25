{-# LANGUAGE OverloadedStrings #-}

module Display (display) where

import Data.Text
import Syntax
import Text.Megaparsec.Pos (SourcePos)

class DisplayI a where
    displayI :: a -> Int -> Text

class Display a where
    display :: a -> Text

instance Display Literal where
    display LUnit = "()"
    display (LBool True) = "true"
    display (LBool False) = "false"
    display (LInt n) = pack $ show n
    display (LFloat f) = pack $ show f

instance Display UnaryOp where
    display Not = "not"
    display Neg = "-"
    display FNeg = "-."

instance Display BinaryOp where
    display (RelationOp Eq) = "="
    display (RelationOp Le) = "<="
    display (RelationOp Ge) = ">="
    display (RelationOp Ne) = "<>"
    display (RelationOp Lt) = "<"
    display (RelationOp Gt) = ">"
    display (IntOp Add) = "+"
    display (IntOp Sub) = "-"
    display (IntOp Mul) = "*"
    display (IntOp Div) = "/"
    display (FloatOp FAdd) = "+."
    display (FloatOp FSub) = "-."
    display (FloatOp FMul) = "*."
    display (FloatOp FDiv) = "/."

instance Display RawIdent where
    display (RawIdent _ ident) = ident

instance Display Ident where
    display (UserDefined _ ident) = ident
    display (CompilerGenerated ident) = ident
    display (ExternalIdent ident) = ident

insertIndent :: Int -> Text
insertIndent depth = Data.Text.replicate depth "    "

instance Display SourcePos where
    display _ = ""

instance (Display state, Display identTy, DisplayI operandTy) => DisplayI (Expr state identTy operandTy) where
    displayI expression indentDepth = withoutState expression indentDepth <> display (getExprState expression)
        where
            withoutState :: (Display state, Display identTy, DisplayI operandTy) => Expr state identTy operandTy -> Int -> Text
            withoutState (Const _ lit) _ = display lit
            withoutState (Unary _ op expr) depth =
                "(" <> display op <> " " <> displayI expr depth <> ")"
            withoutState (Binary _ op expr1 expr2) depth =
                "(" <> displayI expr1 depth <> " " <> display op <> " " <> displayI expr2 depth <> ")"
            withoutState (If _ cond thenExpr elseExpr) depth =
                "(if " <> displayI cond depth <> " then\n"
                    <> insertIndent (depth + 1) <> displayI thenExpr (depth + 1) <> "\n"
                    <> insertIndent depth <> "else\n"
                    <> insertIndent (depth + 1) <> displayI elseExpr (depth + 1) <> ")"
            withoutState (Let _ PUnit value body) depth =
                "(let () = " <> displayI value depth <> " in\n"
                    <> insertIndent (depth + 1) <> displayI body (depth + 1) <> ")"
            withoutState (Let _ (PVar v) value body) depth =
                "(let " <> display v <> " = " <> displayI value depth <> " in\n"
                    <> insertIndent (depth + 1) <> displayI body (depth + 1) <> ")"
            withoutState (Let _ (PRec f args) value body) depth =
                "(let rec " <> display f <> " " <> Data.Text.unwords (Prelude.map display args) <> " = " <> displayI value depth <> " in\n"
                    <> insertIndent (depth + 1) <> displayI body (depth + 1) <> ")"
            withoutState (Let _ (PTuple values) value body) depth =
                "(let " <> Data.Text.unwords (Prelude.map display values) <> " = " <> displayI value depth <> " in\n"
                    <> insertIndent (depth + 1) <> displayI body (depth + 1) <> ")"
            withoutState (App _ func args) depth =
                "(" <> displayI func depth <> " " <> Data.Text.unwords (Prelude.map (`displayI` depth)  args) <> ")"
            withoutState (Tuple _ values) depth =
                "(" <> intercalate ", " (Prelude.map (`displayI` depth) values) <> ")"
            withoutState (ArrayCreate _ size initVal) depth =
                "(Array.create " <> displayI size depth <> " " <> displayI initVal depth <> ")"
            withoutState (Get _ array idx) depth =
                "(" <> displayI array depth <> ".(" <> displayI idx depth <> "))"
            withoutState (Put _ array idx value) depth =
                "(" <> displayI array depth <> ".(" <> displayI idx depth <> ") <- " <> displayI value depth <> ")"
            withoutState (Var _ v) _ = display v

instance DisplayI ParsedExpr where
    displayI (PGuard expr) = displayI expr

instance Display ParsedExpr where
    display expr = displayI expr 0

instance DisplayI ResolvedExpr where
    displayI (RGuard expr) = displayI expr

instance Display ResolvedExpr where
    display expr = displayI expr 0
