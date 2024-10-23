{-# LANGUAGE OverloadedStrings #-}

module Display (displayExpr) where

import Data.Text
import Syntax

displayLiteral :: Literal -> Text
displayLiteral LUnit = "()"
displayLiteral (LBool True) = "true"
displayLiteral (LBool False) = "false"
displayLiteral (LInt n) = pack $ show n
displayLiteral (LFloat f) = pack $ show f

displayUnaryOp :: UnaryOp -> Text
displayUnaryOp Not = "not"
displayUnaryOp Neg = "-"
displayUnaryOp FNeg = "-."

displayBinaryOp :: BinaryOp -> Text
displayBinaryOp (RelationOp Eq) = "="
displayBinaryOp (RelationOp Le) = "<="
displayBinaryOp (RelationOp Ge) = ">="
displayBinaryOp (RelationOp Ne) = "<>"
displayBinaryOp (RelationOp Lt) = "<"
displayBinaryOp (RelationOp Gt) = ">"
displayBinaryOp (IntOp Add) = "+"
displayBinaryOp (IntOp Sub) = "-"
displayBinaryOp (IntOp Mul) = "*"
displayBinaryOp (IntOp Div) = "/"
displayBinaryOp (FloatOp FAdd) = "+."
displayBinaryOp (FloatOp FSub) = "-."
displayBinaryOp (FloatOp FMul) = "*."
displayBinaryOp (FloatOp FDiv) = "/."

displayIdent :: Ident -> Text
displayIdent = snd

displayExpr :: Expr -> Text
displayExpr = displayExpr' 0
  where
    insertIndent :: Int -> Text
    insertIndent depth = Data.Text.replicate depth "    "

    displayExpr' :: Int -> Expr -> Text
    displayExpr' _ (_, Const lit) = displayLiteral lit
    displayExpr' depth (_, Unary op expr) =
        "(" <> displayUnaryOp op <> " " <> displayExpr' depth expr <> ")"
    displayExpr' depth (_, Binary op expr1 expr2) =
        "(" <> displayExpr' depth expr1 <> " " <> displayBinaryOp op <> " " <> displayExpr' depth expr2 <> ")"
    displayExpr' depth (_, If cond thenExpr elseExpr) =
        "(if " <> displayExpr' depth cond <> " then\n"
            <> insertIndent (depth + 1) <> displayExpr' (depth + 1) thenExpr <> "\n"
            <> insertIndent depth <> "else\n"
            <> insertIndent (depth + 1) <> displayExpr' (depth + 1) elseExpr <> ")"
    displayExpr' depth (_, Let binder body) =
        "(let " <> displayBinder depth binder <> " in\n"
            <> insertIndent (depth + 1) <> displayExpr' (depth + 1) body <> ")"
      where
        displayBinder :: Int -> LetBinder -> Text
        displayBinder depth' (LetBinder (PVar v) value) =
            displayIdent v <> " = " <> displayExpr' depth' value
        displayBinder depth' (LetBinder (PRec f args) value) =
            "rec " <> displayIdent f <> " " <> Data.Text.unwords (Prelude.map displayIdent args) <> " = " <> displayExpr' depth' value
        displayBinder depth' (LetBinder (PTuple values) value) =
            Data.Text.unwords (Prelude.map displayIdent values) <> " = " <> displayExpr' depth' value
    displayExpr' depth (_, Then expr1 expr2) =
        "(" <> displayExpr' depth expr1 <> ";\n"
            <> insertIndent (depth + 1) <> displayExpr' (depth + 1) expr2 <> ")"
    displayExpr' _ (_, Var v) = displayIdent v
    displayExpr' depth (_, App func args) =
        "(" <> displayExpr' depth func <> " " <> Data.Text.unwords (Prelude.map (displayExpr' depth) args) <> ")"
    displayExpr' depth (_, Tuple values) =
        "(" <> intercalate ", " (Prelude.map (displayExpr' depth) values) <> ")"
    displayExpr' depth (_, ArrayCreate size initVal) =
        "(Array.create " <> displayExpr' depth size <> " " <> displayExpr' depth initVal <> ")"
    displayExpr' depth (_, Get array idx) =
        "(" <> displayExpr' depth array <> ".(" <> displayExpr' depth idx <> "))"
    displayExpr' depth (_, Put array idx value) =
        "(" <> displayExpr' depth array <> ".(" <> displayExpr' depth idx <> ") <- " <> displayExpr' depth value <> ")"
