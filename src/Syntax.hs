{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Syntax (
    Literal (LUnit, LBool, LInt, LFloat),
    getLiteralType,
    UnaryOp (Not, Neg, FNeg),
    RelationBinOp (Eq, Ne, Lt, Ge),
    IntBinOp (Add, Sub, Mul, Div),
    FloatBinOp (FAdd, FSub, FMul, FDiv),
    BinaryOp (RelationOp, IntOp, FloatOp),
    Ident (Entry, UserDefined, CompilerGenerated, ExternalIdent),
    Pattern (PUnit, PVar, PRec, PTuple),
    Cond (..),
    ParsedExpr (PGuard, pExp),
    ResolvedExpr (RGuard, rExp),
    RawIdent (RawIdent),
    TypedState (TypedState, getType, getLoc),
    fromSourcePos,
    identLoc,
    Loc (Loc, locFileName, locLine, locColumn),
    dummyLoc,
    KExpr,
    TypedExpr (TGuard, tExp),
    ClosureExpr,
    Function (Function, funcName, isDirect, freeVars, boundedArgs, funcBody),
    AllowClosure (AllowClosure),
    DisallowClosure (DisallowClosure),
    AllowCompBranch (AllowCompBranch),
    Expr (..),
    getExprState,
    subst,
    visitExprM,
) where

import Control.Monad.Identity (Identity (runIdentity))
import Data.Text (Text, pack)
import Text.Megaparsec.Pos (SourcePos, sourceColumn, sourceLine, sourceName, unPos)
import Typing (Ty, TypeKind (TBool, TFloat, TInt, TUnit))

data Literal
    = LUnit
    | LBool Bool
    | LInt Int
    | LFloat Float
    deriving (Show, Eq)

getLiteralType :: Literal -> Ty
getLiteralType LUnit = TUnit
getLiteralType (LBool _) = TBool
getLiteralType (LInt _) = TInt
getLiteralType (LFloat _) = TFloat

data UnaryOp
    = Not
    | Neg
    | FNeg
    deriving (Show, Eq)

data RelationBinOp = Eq | Ne | Lt | Ge deriving (Show, Eq)
data IntBinOp = Add | Sub | Mul | Div deriving (Show, Eq)
data FloatBinOp = FAdd | FSub | FMul | FDiv deriving (Show, Eq)
data BinaryOp
    = RelationOp RelationBinOp
    | IntOp IntBinOp
    | FloatOp FloatBinOp
    deriving (Show, Eq)

data RawIdent
    = RawIdent Loc Text
    deriving (Show, Eq)

data Ident
    = -- | The entry of the program.
      Entry Loc
    | -- | An identifier after name resolution.
      UserDefined Loc Text
    | -- | An identifier generated by the compiler.
      CompilerGenerated Int
    | -- | An identifier that is used to refer to an external item.
      ExternalIdent Text
    deriving (Show, Eq)

identLoc :: Ident -> Loc
identLoc (Entry pos) = pos
identLoc (UserDefined pos _) = pos
identLoc (CompilerGenerated _) = Loc "generated" 0 0
identLoc (ExternalIdent _) = Loc "external" 0 0

-- | Pattern matching used in let-expressions.
data Pattern identTy
    = PUnit
    | PVar identTy
    | PRec identTy [identTy]
    | PTuple [identTy]
    deriving (Show, Eq)

data AllowCompBranch = AllowCompBranch
    deriving (Show, Eq)

-- | A condition of an if-expression.
data Cond operandTy allowCompTy where
    CIdentity :: operandTy -> Cond operandTy allowCompTy
    CComp :: RelationBinOp -> operandTy -> operandTy -> Cond operandTy AllowCompBranch

deriving instance
    (Show operandTy, Show allowCompTy) =>
    Show (Cond operandTy allowCompTy)
deriving instance
    (Eq operandTy, Eq allowCompTy) =>
    Eq (Cond operandTy allowCompTy)

{- | The type of an expression after parsing.
PGuard is used for avoiding invalid recursive type definition.
-}
newtype ParsedExpr = PGuard {pExp :: Expr Loc RawIdent ParsedExpr DisallowClosure ()}
    deriving (Show, Eq)

{- | The type of an expression after name resolution.
RGuard is used for avoiding invalid recursive type definition.
-}
newtype ResolvedExpr = RGuard {rExp :: Expr Loc Ident ResolvedExpr DisallowClosure ()}
    deriving (Show, Eq)

data Loc = Loc {locFileName :: Text, locLine :: Int, locColumn :: Int}
    deriving (Show, Eq)

fromSourcePos :: SourcePos -> Loc
fromSourcePos pos = Loc (pack (sourceName pos)) (unPos (sourceLine pos)) (unPos (sourceColumn pos))

dummyLoc :: Loc
dummyLoc = Loc "__dummy" 0 0

data TypedState = TypedState {getType :: Ty, getLoc :: Loc}
    deriving (Show, Eq)

-- | The type of an expression after type inference.
newtype TypedExpr = TGuard {tExp :: Expr TypedState Ident TypedExpr DisallowClosure ()}
    deriving (Show, Eq)

-- | The type of an expression after K-normalization.
type KExpr = Expr TypedState Ident Ident DisallowClosure AllowCompBranch

-- | The type of an expression after introducing closures.
type ClosureExpr = Expr TypedState Ident Ident AllowClosure AllowCompBranch

data Function = Function
    { funcState :: TypedState
    , isDirect :: Bool
    , funcName :: Ident
    , freeVars :: [Ident]
    , boundedArgs :: [Ident]
    , funcBody :: ClosureExpr
    }
    deriving (Show, Eq)

data AllowClosure = AllowClosure
    deriving (Show, Eq)

data DisallowClosure = DisallowClosure
    deriving (Show, Eq)

data Expr state identTy operandTy closureTy branchTy where
    Const ::
        state ->
        Literal ->
        Expr state a b closureTy branchTy
    Unary ::
        state ->
        UnaryOp ->
        operandTy ->
        Expr state a operandTy closureTy branchTy
    Binary ::
        state ->
        BinaryOp ->
        operandTy ->
        operandTy ->
        Expr state a operandTy closureTy branchTy
    If ::
        state ->
        Cond operandTy branchTy ->
        Expr state identTy operandTy closureTy branchTy ->
        Expr state identTy operandTy closureTy branchTy ->
        Expr state identTy operandTy closureTy branchTy
    Let ::
        state ->
        Pattern identTy ->
        Expr state identTy operandTy closureTy branchTy ->
        Expr state identTy operandTy closureTy branchTy ->
        Expr state identTy operandTy closureTy branchTy
    Var ::
        state ->
        identTy ->
        Expr state identTy b closureTy branchTy
    App ::
        state ->
        operandTy ->
        [operandTy] ->
        Expr state identTy operandTy DisallowClosure branchTy
    Tuple ::
        state ->
        [operandTy] ->
        Expr state a operandTy closureTy branchTy
    ArrayCreate ::
        state ->
        operandTy ->
        operandTy ->
        Expr state identTy operandTy closureTy branchTy
    Get ::
        state ->
        operandTy ->
        operandTy ->
        Expr state a operandTy closureTy branchTy
    Put ::
        state ->
        operandTy ->
        operandTy ->
        operandTy ->
        Expr state identTy operandTy closureTy branchTy
    MakeClosure ::
        state ->
        identTy ->
        [operandTy] ->
        Expr state identTy operandTy AllowClosure branchTy
    ClosureApp ::
        state ->
        identTy ->
        [operandTy] ->
        Expr state identTy operandTy AllowClosure branchTy
    DirectApp ::
        state ->
        identTy ->
        [operandTy] ->
        Expr state identTy operandTy AllowClosure branchTy

deriving instance
    (Show state, Show identTy, Show operandTy, Show closureTy, Show branchTy) =>
    Show (Expr state identTy operandTy closureTy branchTy)
deriving instance
    (Eq state, Eq identTy, Eq operandTy, Eq closureTy, Eq branchTy) =>
    Eq (Expr state identTy operandTy closureTy branchTy)

getExprState :: Expr state identTy operandTy closureTy branchTy -> state
getExprState (Const state _) = state
getExprState (Unary state _ _) = state
getExprState (Binary state _ _ _) = state
getExprState (If state _ _ _) = state
getExprState (Let state _ _ _) = state
getExprState (Var state _) = state
getExprState (App state _ _) = state
getExprState (Tuple state _) = state
getExprState (ArrayCreate state _ _) = state
getExprState (Get state _ _) = state
getExprState (Put state _ _ _) = state
getExprState (MakeClosure state _ _) = state
getExprState (ClosureApp state _ _) = state
getExprState (DirectApp state _ _) = state

-- | Substitute identifiers and operands. The identifiers should be unique.
subst ::
    (Eq identTy, Eq operandTy) =>
    identTy ->
    identTy ->
    operandTy ->
    operandTy ->
    Expr state identTy operandTy closureTy branchTy ->
    Expr state identTy operandTy closureTy branchTy
subst iBefore iAfter oBefore oAfter expr =
    runIdentity $
        visitExprM
            pure
            (\ident -> pure $ if ident == iBefore then iAfter else ident)
            (\operand -> pure $ if operand == oBefore then oAfter else operand)
            expr

visitExprM ::
    (Monad m) =>
    (state -> m state') ->
    (identTy -> m identTy') ->
    (operandTy -> m operandTy') ->
    Expr state identTy operandTy closureTy branchTy ->
    m (Expr state' identTy' operandTy' closureTy branchTy)
visitExprM fState _ _ (Const state lit) = do
    state' <- fState state
    pure $ Const state' lit
visitExprM fState _ fOperand (Unary state op operand) = do
    state' <- fState state
    operand' <- fOperand operand
    pure $ Unary state' op operand'
visitExprM fState _ fOperand (Binary state op lhs rhs) = do
    state' <- fState state
    lhs' <- fOperand lhs
    rhs' <- fOperand rhs
    pure $ Binary state' op lhs' rhs'
visitExprM fState fIdent fOperand (If state (CIdentity cond) thenE elseE) = do
    state' <- fState state
    cond' <- fOperand cond
    thenE' <- visitExprM fState fIdent fOperand thenE
    elseE' <- visitExprM fState fIdent fOperand elseE
    pure $ If state' (CIdentity cond') thenE' elseE'
visitExprM fState fIdent fOperand (If state (CComp op lhs rhs) thenE elseE) = do
    state' <- fState state
    lhs' <- fOperand lhs
    rhs' <- fOperand rhs
    thenE' <- visitExprM fState fIdent fOperand thenE
    elseE' <- visitExprM fState fIdent fOperand elseE
    pure $ If state' (CComp op lhs' rhs') thenE' elseE'
visitExprM fState fIdent fOperand (Let state pat expr body) = do
    state' <- fState state
    expr' <- visitExprM fState fIdent fOperand expr
    body' <- visitExprM fState fIdent fOperand body
    pat' <- visitPatternM fIdent pat
    pure $ Let state' pat' expr' body'
  where
    visitPatternM :: (Monad m) => (identTy -> m identTy') -> Pattern identTy -> m (Pattern identTy')
    visitPatternM _ PUnit = pure PUnit
    visitPatternM fIdent' (PVar ident) = PVar <$> fIdent' ident
    visitPatternM fIdent' (PRec ident idents) = PRec <$> fIdent' ident <*> mapM fIdent' idents
    visitPatternM fIdent' (PTuple idents) = PTuple <$> mapM fIdent' idents
visitExprM fState fIdent _ (Var state ident) = do
    state' <- fState state
    ident' <- fIdent ident
    pure $ Var state' ident'
visitExprM fState _ fOperand (App state func args) = do
    state' <- fState state
    func' <- fOperand func
    args' <- mapM fOperand args
    pure $ App state' func' args'
visitExprM fState _ fOperand (Tuple state values) = do
    state' <- fState state
    values' <- mapM fOperand values
    pure $ Tuple state' values'
visitExprM fState _ fOperand (ArrayCreate state size value) = do
    state' <- fState state
    size' <- fOperand size
    value' <- fOperand value
    pure $ ArrayCreate state' size' value'
visitExprM fState _ fOperand (Get state array index) = do
    state' <- fState state
    array' <- fOperand array
    index' <- fOperand index
    pure $ Get state' array' index'
visitExprM fState _ fOperand (Put state array index value) = do
    state' <- fState state
    array' <- fOperand array
    index' <- fOperand index
    value' <- fOperand value
    pure $ Put state' array' index' value'
visitExprM fState fIdent fOperand (MakeClosure state ident args) = do
    state' <- fState state
    ident' <- fIdent ident
    args' <- mapM fOperand args
    pure $ MakeClosure state' ident' args'
visitExprM fState fIdent fOperand (ClosureApp state ident args) = do
    state' <- fState state
    ident' <- fIdent ident
    args' <- mapM fOperand args
    pure $ ClosureApp state' ident' args'
visitExprM fState fIdent fOperand (DirectApp state ident args) = do
    state' <- fState state
    ident' <- fIdent ident
    args' <- mapM fOperand args
    pure $ DirectApp state' ident' args'
