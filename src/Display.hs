{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Display (display) where

import Data.Text
import Syntax
import Text.Megaparsec.Pos (SourcePos (sourceColumn, sourceLine), unPos)
import Typing

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

instance DisplayI Ident where
    displayI (UserDefined pos ident) _ =
        "__"
            <> (pack . show . unPos . sourceLine) pos
            <> "_"
            <> (pack . show . unPos . sourceColumn) pos
            <> "_"
            <> ident
    displayI (CompilerGenerated ident) _ =
        "__gen_" <> pack (show ident)
    displayI (ExternalIdent ident) _ =
        "__ext_" <> ident

instance Display Ident where
    display ident = displayI ident 0

insertIndent :: Int -> Text
insertIndent depth = Data.Text.replicate depth "    "

instance Display SourcePos where
    display _ = ""

instance Display TypedState where
    display (TypedState ty _) = " (* : " <> display ty <> " *)"

instance (Display state, Display identTy, DisplayI operandTy) => DisplayI (Expr state identTy operandTy closureTy) where
    displayI expression indentDepth = withoutState expression indentDepth <> display (getExprState expression)
      where
        withoutState :: (Display state, Display identTy, DisplayI operandTy) => Expr state identTy operandTy closureTy -> Int -> Text
        withoutState (Const _ lit) _ = display lit
        withoutState (Unary _ op expr) depth =
            "(" <> display op <> " " <> displayI expr depth <> ")"
        withoutState (Binary _ op expr1 expr2) depth =
            "(" <> displayI expr1 depth <> " " <> display op <> " " <> displayI expr2 depth <> ")"
        withoutState (If _ cond thenExpr elseExpr) depth =
            "(if "
                <> displayI cond depth
                <> " then\n"
                <> insertIndent (depth + 1)
                <> displayI thenExpr (depth + 1)
                <> "\n"
                <> insertIndent depth
                <> "else\n"
                <> insertIndent (depth + 1)
                <> displayI elseExpr (depth + 1)
                <> ")"
        withoutState (Let _ PUnit value body) depth =
            "(let () = "
                <> displayI value depth
                <> " in\n"
                <> insertIndent (depth + 1)
                <> displayI body (depth + 1)
                <> ")"
        withoutState (Let _ (PVar v) value body) depth =
            "(let "
                <> display v
                <> " = "
                <> displayI value depth
                <> " in\n"
                <> insertIndent (depth + 1)
                <> displayI body (depth + 1)
                <> ")"
        withoutState (Let _ (PRec f args) value body) depth =
            "(let rec "
                <> display f
                <> " "
                <> Data.Text.unwords (Prelude.map display args)
                <> " = "
                <> displayI value depth
                <> " in\n"
                <> insertIndent (depth + 1)
                <> displayI body (depth + 1)
                <> ")"
        withoutState (Let _ (PTuple values) value body) depth =
            "(let "
                <> Data.Text.unwords (Prelude.map display values)
                <> " = "
                <> displayI value depth
                <> " in\n"
                <> insertIndent (depth + 1)
                <> displayI body (depth + 1)
                <> ")"
        withoutState (App _ func args) depth =
            "(" <> displayI func depth <> " " <> Data.Text.unwords (Prelude.map (`displayI` depth) args) <> ")"
        withoutState (Tuple _ values) depth =
            "(" <> intercalate ", " (Prelude.map (`displayI` depth) values) <> ")"
        withoutState (ArrayCreate _ size initVal) depth =
            "(Array.create " <> displayI size depth <> " " <> displayI initVal depth <> ")"
        withoutState (Get _ array idx) depth =
            "(" <> displayI array depth <> ".(" <> displayI idx depth <> "))"
        withoutState (Put _ array idx value) depth =
            "(" <> displayI array depth <> ".(" <> displayI idx depth <> ") <- " <> displayI value depth <> ")"
        withoutState (Var _ v) _ = display v
        withoutState (MakeClosure _ ident args) depth =
            "<" <> display ident <> ", " <> Data.Text.unwords (Prelude.map (`displayI` depth) args) <> ">"
        withoutState (ClosureApp _ closure args) depth =
            "(" <> display closure <> " @ " <> Data.Text.unwords (Prelude.map (`displayI` depth) args) <> ")"
        withoutState (DirectApp _ func args) depth =
            "(" <> display func <> " " <> Data.Text.unwords (Prelude.map (`displayI` depth) args) <> ")"

instance (Display state, Display identTy, DisplayI operandTy) => Display (Expr state identTy operandTy closureTy) where
    display expr = displayI expr 0

instance DisplayI ParsedExpr where
    displayI (PGuard expr) = displayI expr

instance Display ParsedExpr where
    display expr = displayI expr 0

instance DisplayI ResolvedExpr where
    displayI (RGuard expr) = displayI expr

instance Display ResolvedExpr where
    display expr = displayI expr 0

instance DisplayI TypedExpr where
    displayI (TGuard expr) = displayI expr

instance Display TypedExpr where
    display expr = displayI expr 0

instance DisplayI Function where
    displayI func depth =
        displayI (funcName func) depth
            <> "@"
            <> (if isDirect func then "direct" else "closure")
            <> " {"
            <> Data.Text.unwords (Prelude.map display (freeVars func))
            <> "} "
            <> Data.Text.unwords (Prelude.map display (boundedArgs func))
            <> ":\n"
            <> insertIndent (depth + 1)
            <> displayI (funcBody func) (depth + 1)

instance Display Function where
    display func = displayI func 0

instance Display (TypeKind a) where
    display TUnit = "unit"
    display TBool = "bool"
    display TInt = "int"
    display TFloat = "float"
    display (TFun args ret) =
        "(" <> intercalate ", " (Prelude.map display args) <> ") -> " <> display ret
    display (TTuple values) =
        "(" <> intercalate " * " (Prelude.map display values) <> ")"
    display (TArray value) =
        display value <> " array"
    display (TVar tId) = "__t" <> pack (show tId)
