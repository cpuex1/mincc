{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Display (display) where

import Backend.Asm
import Data.Text
import Syntax
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
    display (RelationOp Ge) = ">="
    display (RelationOp Ne) = "<>"
    display (RelationOp Lt) = "<"
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
            <> (pack . show . locLine) pos
            <> "_"
            <> (pack . show . locColumn) pos
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

instance Display Loc where
    display _ = ""

instance Display TypedState where
    display (TypedState ty _) = " (* : " <> display ty <> " *)"

instance (Display state, Display identTy, DisplayI operandTy) => DisplayI (Expr state identTy operandTy closureTy branchTy) where
    displayI expression indentDepth = withoutState expression indentDepth <> display (getExprState expression)
      where
        withoutState :: (Display state, Display identTy, DisplayI operandTy) => Expr state identTy operandTy closureTy branchTy -> Int -> Text
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
        withoutState (IfComp _ op left right thenExpr elseExpr) depth =
            "(if "
                <> "("
                <> displayI left depth
                <> " "
                <> display (RelationOp op)
                <> " "
                <> displayI right depth
                <> ")"
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

instance (Display state, Display identTy, DisplayI operandTy) => Display (Expr state identTy operandTy closureTy branchTy) where
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

instance Display (Register Int Int) where
    display ZeroReg = "zero"
    display RetReg = "a0"
    display HeapReg = "hp"
    display StackReg = "sp"
    display (ArgsReg idTy) = "a" <> pack (show idTy)
    display (TempReg idTy) = "t" <> pack (show idTy)

instance Display (Register Int Float) where
    display ZeroReg = "fz"
    display RetReg = "f0"
    display (ArgsReg idTy) = "f" <> pack (show idTy)
    display (TempReg idTy) = "ft" <> pack (show idTy)

instance DisplayI (Inst stateTy Int branchTy) where
    displayI (ICompOp _ op lhs rhs1 rhs2) _ =
        case rhs2 of
            (Reg rhs2') ->
                toOp op <> " " <> display lhs <> ", " <> display rhs1 <> ", " <> display rhs2'
            (Imm rhs2') ->
                toOp op <> "i " <> display lhs <> ", " <> display rhs1 <> ", " <> pack (show rhs2')
      where
        toOp :: RelationBinOp -> Text
        toOp Eq = "seq"
        toOp Ne = "sne"
        toOp Ge = "sge"
        toOp Lt = "slt"
    displayI (IFCompOp _ op lhs rhs1 rhs2) _ =
        toOp op <> display lhs <> ", " <> display rhs1 <> ", " <> display rhs2
      where
        toOp :: RelationBinOp -> Text
        toOp Eq = "feq"
        toOp Ne = "fne"
        toOp Ge = "fge"
        toOp Lt = "flt"
    displayI (IIntOp _ op lhs rhs1 rhs2) _ =
        case rhs2 of
            (Reg rhs2') ->
                toOp op <> " " <> display lhs <> ", " <> display rhs1 <> ", " <> display rhs2'
            (Imm rhs2') ->
                toOp op <> "i " <> display lhs <> ", " <> display rhs1 <> ", " <> pack (show rhs2')
      where
        toOp :: IntBinOp -> Text
        toOp Add = "add"
        toOp Sub = "sub"
        toOp Mul = "mul"
        toOp Div = "div"
    displayI (IFOp _ op lhs rhs1 rhs2) _ =
        toOp op <> " " <> display lhs <> ", " <> display rhs1 <> ", " <> display rhs2
      where
        toOp :: FloatBinOp -> Text
        toOp FAdd = "fadd"
        toOp FSub = "fsub"
        toOp FMul = "fmul"
        toOp FDiv = "fdiv"
    displayI (IMov _ lhs (Reg rhs)) _ =
        "mov " <> display lhs <> ", " <> display rhs
    displayI (IMov _ lhs (Imm rhs)) _ =
        "movi " <> display lhs <> ", " <> pack (show rhs)
    displayI (IFMov _ lhs (Reg rhs)) _ =
        "fmov " <> display lhs <> ", " <> display rhs
    displayI (IFMov _ lhs (Imm rhs)) _ =
        "fmovi " <> display lhs <> ", " <> pack (show rhs)
    displayI (IRichCall _ func args fArgs) _ =
        "call! "
            <> func
            <> ", "
            <> Data.Text.intercalate ", " (Prelude.map display args)
            <> ", "
            <> Data.Text.intercalate ", " (Prelude.map display fArgs)
    displayI (IClosureCall _ func args fArgs) _ =
        "clcall! "
            <> display func
            <> ", ["
            <> Data.Text.intercalate ", " (Prelude.map display args)
            <> "], ["
            <> Data.Text.intercalate ", " (Prelude.map display fArgs)
            <> "]"
    displayI (IMakeClosure _ dest func args fArgs) _ =
        "clmake! "
            <> display dest
            <> ", "
            <> func
            <> ", ["
            <> Data.Text.intercalate "," (Prelude.map display args)
            <> "], ["
            <> Data.Text.intercalate "," (Prelude.map display fArgs)
            <> "]"
    displayI (ICall _ func) _ =
        "call " <> func
    displayI (ILoad _ lhs rhs offset) _ =
        "lw " <> display lhs <> ", " <> pack (show offset) <> "(" <> display rhs <> ")"
    displayI (IStore _ lhs rhs offset) _ =
        "sw " <> display lhs <> ", " <> pack (show offset) <> "(" <> display rhs <> ")"
    displayI (IFLoad _ lhs rhs offset) _ =
        "flw " <> display lhs <> ", " <> pack (show offset) <> "(" <> display rhs <> ")"
    displayI (IFStore _ lhs rhs offset) _ =
        "fsw " <> display lhs <> ", " <> pack (show offset) <> "(" <> display rhs <> ")"
    displayI (IBranch _ op lhs rhs thenInst elseInst) depth =
        toOp op
            <> " "
            <> display lhs
            <> ", "
            <> display rhs
            <> Data.Text.intercalate "" (Prelude.map (\i -> "\n" <> insertIndent (depth + 1) <> displayI i (depth + 1)) thenInst)
            <> "\n"
            <> insertIndent depth
            <> "else!"
            <> Data.Text.intercalate "" (Prelude.map (\i -> "\n" <> insertIndent (depth + 1) <> displayI i (depth + 1)) elseInst)
      where
        toOp :: RelationBinOp -> Text
        toOp Eq = "ifeq!"
        toOp Ne = "ifne!"
        toOp Ge = "ifge!"
        toOp Lt = "iflt!"

instance Display (Inst stateTy Int branchTy) where
    display inst = displayI inst 0

instance Display (InstTerm stateTy Int) where
    display (Return _) = "ret"
    display (Jmp _ label) = "jmp " <> label
    display (Branch _ Eq lhs rhs label1 _) =
        "beq " <> display lhs <> ", " <> display rhs <> ", " <> label1
    display (Branch _ Ge lhs rhs label1 _) =
        "bge " <> display lhs <> ", " <> display rhs <> ", " <> label1
    display (Branch _ Ne lhs rhs label1 _) =
        "bne " <> display lhs <> ", " <> display rhs <> ", " <> label1
    display (Branch _ Lt lhs rhs label1 _) =
        "blt " <> display lhs <> ", " <> display rhs <> ", " <> label1
    display Nop = "nop"

instance Display (IntermediateCodeBlock stateTy Int) where
    display (IntermediateCodeBlock label inst) =
        label
            <> ":\n"
            <> intercalate "\n" (Prelude.map (\i -> insertIndent 1 <> displayI i 1) inst)

instance Display (CodeBlock stateTy Int) where
    display (CodeBlock label inst term) =
        label
            <> ":\n"
            <> intercalate "" (Prelude.map (\i -> insertIndent 1 <> display i <> "\n") inst)
            <> insertIndent 1
            <> display term
