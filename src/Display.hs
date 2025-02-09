{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Display (display, Display, displayI, DisplayI) where

import BackEnd.Liveness (Liveness (Liveness), LivenessLoc (livenessProp))
import Data.Set (toAscList)
import Data.Text
import IR
import Numeric (showFFloat)
import Registers
import Syntax
import Typing

class DisplayI a where
    displayI :: a -> Int -> Text

class Display a where
    display :: a -> Text

instance Display Int where
    display = pack . show

instance Display Literal where
    display LUnit = "()"
    display (LBool True) = "true"
    display (LBool False) = "false"
    display (LInt n) = pack $ show n
    display (LFloat f) = pack $ showFFloat Nothing f ""

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
    displayI (Entry _) _ = "__entry"
    displayI (UserDefined pos ident) _ =
        "__"
            <> ident
            <> "_"
            <> (pack . show . locLine) pos
            <> "_"
            <> (pack . show . locColumn) pos
    displayI (CompilerGenerated ident) _ =
        "__gen_" <> pack (show ident)
    displayI (ExternalIdent ident) _ =
        "__ext_" <> ident

instance Display Ident where
    display ident = displayI ident 0

insertIndent :: Int -> Text
insertIndent depth = Data.Text.replicate depth "    "

instance Display Loc where
    display loc = locFileName loc <> ":" <> pack (show (locLine loc)) <> ":" <> pack (show (locColumn loc))

instance Display TypedState where
    display (TypedState ty _) = ": " <> display ty

instance (Display (StateTy kind), Display (IdentTy kind), DisplayI (OperandTy kind)) => DisplayI (Expr kind) where
    displayI expression indentDepth = withoutState expression indentDepth <> " (* " <> display (getExprState expression) <> " *)"
      where
        withoutState :: (Display (StateTy kind), Display (IdentTy kind), DisplayI (OperandTy kind)) => Expr kind -> Int -> Text
        withoutState (Const _ lit) _ = display lit
        withoutState (Unary _ op expr) depth =
            "(" <> display op <> " " <> displayI expr depth <> ")"
        withoutState (Binary _ op expr1 expr2) depth =
            "(" <> displayI expr1 depth <> " " <> display op <> " " <> displayI expr2 depth <> ")"
        withoutState (If _ (CIdentity cond) thenExpr elseExpr) depth =
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
        withoutState (If _ (CComp op lhs rhs) thenExpr elseExpr) depth =
            "(if "
                <> "("
                <> displayI lhs depth
                <> " "
                <> display (RelationOp op)
                <> " "
                <> displayI rhs depth
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
                <> insertIndent depth
                <> displayI body depth
                <> ")"
        withoutState (Let _ (PVar v) value body) depth =
            "(let "
                <> display v
                <> " = "
                <> displayI value depth
                <> " in\n"
                <> insertIndent depth
                <> displayI body depth
                <> ")"
        withoutState (Let _ (PRec f args) value body) depth =
            "(let rec "
                <> display f
                <> " "
                <> Data.Text.unwords (Prelude.map display args)
                <> " =\n"
                <> insertIndent (depth + 1)
                <> displayI value (depth + 1)
                <> " in\n"
                <> insertIndent depth
                <> displayI body depth
                <> ")"
        withoutState (Let _ (PTuple values) value body) depth =
            "(let ("
                <> Data.Text.unwords (Prelude.map display values)
                <> ") = "
                <> displayI value depth
                <> " in\n"
                <> insertIndent depth
                <> displayI body depth
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
        withoutState (Loop _ args values body) depth =
            "(loop "
                <> Data.Text.intercalate ", " (Prelude.zipWith (\a v -> display a <> " := " <> display v) args values)
                <> "\n"
                <> insertIndent (depth + 1)
                <> displayI body (depth + 1)
                <> ")"
        withoutState (Continue _ values) _ = "continue " <> Data.Text.unwords (Prelude.map display values)
        withoutState (MakeClosure _ ident args) depth =
            "<" <> display ident <> ", " <> Data.Text.unwords (Prelude.map (`displayI` depth) args) <> ">"
        withoutState (ClosureApp _ closure args) depth =
            "(" <> display closure <> " @ " <> Data.Text.unwords (Prelude.map (`displayI` depth) args) <> ")"
        withoutState (DirectApp _ func args) depth =
            "(" <> display func <> " " <> Data.Text.unwords (Prelude.map (`displayI` depth) args) <> ")"

instance (DisplayI (Expr kind)) => Display (Expr kind) where
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

instance Display (Register RegID a) where
    display (Register RInt ZeroReg) = "zero"
    display (Register RFloat ZeroReg) = "fzero"
    display (Register _ ReturnReg) = "ra"
    display (Register RInt RetReg) = "a0"
    display (Register RFloat RetReg) = "fa0"
    display (Register _ HeapReg) = "hp"
    display (Register _ StackReg) = "sp"
    display (Register RInt (ArgsReg idTy)) = "a" <> pack (show idTy)
    display (Register RFloat (ArgsReg idTy)) = "fa" <> pack (show idTy)
    display (Register RInt (TempReg idTy)) = "t" <> pack (show idTy)
    display (Register RFloat (TempReg idTy)) = "ft" <> pack (show idTy)
    display (Register RInt (SavedReg idTy)) = "s" <> pack (show idTy)
    display (Register RFloat (SavedReg idTy)) = "fs" <> pack (show idTy)
    display (Register RInt (GeneralReg idTy)) = "r" <> pack (show idTy)
    display (Register RFloat (GeneralReg idTy)) = "fr" <> pack (show idTy)
    display (Register RInt DCReg) = "zero" -- Don't care
    display (Register RFloat DCReg) = "fzero" -- Don't care

instance Display (RegOrImm RegID Int) where
    display (Reg reg) = display reg
    display (Imm _ imm) = pack (show imm)

instance Display (RegOrImm RegID Float) where
    display (Reg reg) = display reg
    display (Imm _ imm) = pack (show imm)

instructionWidth :: Int
instructionWidth = 30

instance (Display (InstStateTy ty), RegIDTy ty ~ RegID) => DisplayI (Inst ty) where
    displayI inst depth =
        case inst of
            (IBranch{}) -> displayed
            _ -> justifyLeft instructionWidth ' ' displayed <> " # " <> display (getIState inst)
      where
        displayed = withoutState inst depth

        withoutState :: (Display (InstStateTy ty), RegIDTy ty ~ RegID) => Inst ty -> Int -> Text
        withoutState (ICompOp _ op lhs rhs1@(Register rTy _) rhs2) _ =
            case rhs2 of
                (Reg rhs2') ->
                    prefix rTy <> toOp op <> " " <> display lhs <> ", " <> display rhs1 <> ", " <> display rhs2'
                (Imm RInt rhs2') ->
                    prefix rTy <> toOp op <> "i " <> display lhs <> ", " <> display rhs1 <> ", " <> pack (show rhs2')
                (Imm RFloat rhs2') ->
                    prefix rTy <> toOp op <> "i " <> display lhs <> ", " <> display rhs1 <> ", " <> pack (show rhs2')
          where
            prefix :: RegType a -> Text
            prefix RInt = "s"
            prefix RFloat = "f"

            toOp :: RelationBinOp -> Text
            toOp Eq = "eq"
            toOp Ne = "ne"
            toOp Ge = "ge"
            toOp Lt = "lt"
        withoutState (IIntOp _ op lhs rhs1 rhs2) _ =
            case rhs2 of
                (Reg rhs2') ->
                    toOp op <> " " <> display lhs <> ", " <> display rhs1 <> ", " <> display rhs2'
                (Imm _ rhs2') ->
                    toOp op <> "i " <> display lhs <> ", " <> display rhs1 <> ", " <> pack (show rhs2')
          where
            toOp :: PrimitiveIntOp -> Text
            toOp PAdd = "add"
            toOp PSub = "sub"
            toOp PMul = "mul"
            toOp PDiv = "div"
            toOp PAnd = "and"
            toOp POr = "or"
            toOp PXor = "xor"
            toOp PShiftL = "sll"
            toOp PShiftR = "srl"
        withoutState (IFOp _ op lhs rhs1 rhs2) _ =
            toOp op <> " " <> display lhs <> ", " <> display rhs1 <> ", " <> display rhs2
          where
            toOp :: FloatBinOp -> Text
            toOp FAdd = "fadd"
            toOp FSub = "fsub"
            toOp FMul = "fmul"
            toOp FDiv = "fdiv"
        withoutState (IMov _ lhs rhs@(Reg (Register RInt _))) _ =
            "mov " <> display lhs <> ", " <> display rhs
        withoutState (IMov _ lhs rhs@(Imm RInt _)) _ =
            "movi " <> display lhs <> ", " <> display rhs
        withoutState (IMov _ lhs rhs@(Reg (Register RFloat _))) _ =
            "fmov " <> display lhs <> ", " <> display rhs
        withoutState (IMov _ lhs rhs@(Imm RFloat _)) _ =
            "fmovi " <> display lhs <> ", " <> display rhs
        withoutState (ILMov _ lhs rhs) _ =
            "movi " <> display lhs <> ", " <> rhs
        withoutState (IRichCall _ func args fArgs) _ =
            "call! "
                <> func
                <> ", ["
                <> Data.Text.intercalate ", " (Prelude.map display args)
                <> "], ["
                <> Data.Text.intercalate ", " (Prelude.map display fArgs)
                <> "]"
        withoutState (IClosureCall _ func args fArgs) _ =
            "clcall! "
                <> display func
                <> ", ["
                <> Data.Text.intercalate ", " (Prelude.map display args)
                <> "], ["
                <> Data.Text.intercalate ", " (Prelude.map display fArgs)
                <> "]"
        withoutState (IMakeClosure _ dest func args fArgs) _ =
            "clmake! "
                <> display dest
                <> ", "
                <> func
                <> ", ["
                <> Data.Text.intercalate "," (Prelude.map display args)
                <> "], ["
                <> Data.Text.intercalate "," (Prelude.map display fArgs)
                <> "]"
        withoutState (ICall _ func) _ =
            "call " <> func
        withoutState (ICallReg _ reg) _ =
            "callr " <> display reg
        withoutState (ILoad _ lhs@(Register RInt _) rhs offset) _ =
            "lw " <> display lhs <> ", " <> pack (show offset) <> "(" <> display rhs <> ")"
        withoutState (IStore _ lhs@(Register RInt _) rhs offset) _ =
            "sw " <> display lhs <> ", " <> pack (show offset) <> "(" <> display rhs <> ")"
        withoutState (ILoad _ lhs@(Register RFloat _) rhs offset) _ =
            "flw " <> display lhs <> ", " <> pack (show offset) <> "(" <> display rhs <> ")"
        withoutState (IStore _ lhs@(Register RFloat _) rhs offset) _ =
            "fsw " <> display lhs <> ", " <> pack (show offset) <> "(" <> display rhs <> ")"
        withoutState (IRawInst _ name ret iArgs fArgs) _ =
            name
                <> " "
                <> Data.Text.intercalate ", " (display ret : Prelude.map display iArgs ++ Prelude.map display fArgs)
        withoutState (IBranch _ op lhs rhs thenInst elseInst) depth' =
            toOp op
                <> " "
                <> display lhs
                <> ", "
                <> display rhs
                <> Data.Text.intercalate "" (Prelude.map (\i -> "\n" <> insertIndent (depth' + 1) <> displayI i (depth' + 1)) thenInst)
                <> "\n"
                <> insertIndent depth'
                <> "else!"
                <> Data.Text.intercalate "" (Prelude.map (\i -> "\n" <> insertIndent (depth' + 1) <> displayI i (depth' + 1)) elseInst)
          where
            toOp :: RelationBinOp -> Text
            toOp Eq = "ifeq!"
            toOp Ne = "ifne!"
            toOp Ge = "ifge!"
            toOp Lt = "iflt!"
        withoutState (ILoop _ iArgs fArgs iValues fValues body) depth' =
            "loop! "
                <> Data.Text.intercalate
                    ", "
                    ( Prelude.zipWith (\a v -> display a <> " := " <> display v) iArgs iValues
                        ++ Prelude.zipWith (\a v -> display a <> " := " <> display v) fArgs fValues
                    )
                <> Data.Text.intercalate "" (Prelude.map (\i -> "\n" <> insertIndent (depth' + 1) <> displayI i (depth' + 1)) body)
        withoutState (IContinue _ iArgs fArgs) _ =
            "continue! "
                <> Data.Text.intercalate ", " (Prelude.map display iArgs ++ Prelude.map display fArgs)

instance (Display (InstStateTy ty), RegIDTy ty ~ RegID) => Display (Inst ty) where
    display inst = displayI inst 0

instance (RegIDTy ty ~ RegID) => Display (InstTerm ty) where
    display Return = "ret"
    display (Jmp label) = "jmp " <> label
    display (Branch _ Eq lhs@(Register RInt _) rhs label) =
        "beq " <> display lhs <> ", " <> display rhs <> ", " <> label
    display (Branch _ Ge lhs@(Register RInt _) rhs label) =
        "bge " <> display lhs <> ", " <> display rhs <> ", " <> label
    display (Branch _ Ne lhs@(Register RInt _) rhs label) =
        "bne " <> display lhs <> ", " <> display rhs <> ", " <> label
    display (Branch _ Lt lhs@(Register RInt _) rhs label) =
        "blt " <> display lhs <> ", " <> display rhs <> ", " <> label
    display (Branch _ Eq lhs@(Register RFloat _) rhs label) =
        "bfeq " <> display lhs <> ", " <> display rhs <> ", " <> label
    display (Branch _ Ge lhs@(Register RFloat _) rhs label) =
        "bfge " <> display lhs <> ", " <> display rhs <> ", " <> label
    display (Branch _ Ne lhs@(Register RFloat _) rhs label) =
        "bfne " <> display lhs <> ", " <> display rhs <> ", " <> label
    display (Branch _ Lt lhs@(Register RFloat _) rhs label) =
        "bflt " <> display lhs <> ", " <> display rhs <> ", " <> label
    display Nop = "nop"

instance Display (RegVariant Liveness) where
    display (RegVariant (Liveness iAlive) (Liveness fAlive)) =
        "["
            <> Data.Text.intercalate "," (Prelude.map (display . Register RInt . SavedReg) (toAscList iAlive))
            <> "], ["
            <> Data.Text.intercalate "," (Prelude.map (display . Register RFloat . SavedReg) (toAscList fAlive))
            <> "]"

instance Display LivenessLoc where
    display loc = display $ livenessProp loc

instance (Display (InstStateTy ty), RegIDTy ty ~ RegID) => Display (HCodeBlock ty) where
    display (HCodeBlock label _ inst) =
        label
            <> ":\n"
            <> intercalate "\n" (Prelude.map (\i -> insertIndent 1 <> displayI i 1) inst)

instance (Display (InstStateTy ty), RegIDTy ty ~ RegID) => Display (LCodeBlock ty) where
    display (LCodeBlock label inst term) =
        label
            <> ":"
            <> intercalate "" (Prelude.map (\i -> "\n" <> insertIndent 1 <> display i) inst)
            <> ( case term of
                    Nop -> ""
                    _ -> "\n" <> insertIndent 1 <> display term
               )
