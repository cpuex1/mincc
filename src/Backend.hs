{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend (
    BackendConfig (BackendConfig),
    toInstructions,
) where

import Asm
import Control.Monad (when)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Reader (MonadTrans (lift), ReaderT (runReaderT), asks)
import Control.Monad.State (MonadState (get), StateT (runStateT), gets, modify)
import Display (display)
import Error (CompilerError (OtherError))
import IdentAnalysis (IdentEnvT, getTyOf)
import Syntax
import Typing (TypeKind (TFloat, TUnit))

type RegID = Int

newtype BackendConfig = BackendConfig
    { argLimit :: Int
    }
    deriving (Show, Eq)

data BackendEnv = BackendEnv
    { generatedIReg :: Int
    , generatedFReg :: Int
    , iMap :: [(Ident, Register RegID Int)]
    , fMap :: [(Ident, Register RegID Float)]
    }
    deriving (Show, Eq)

defaultBackendEnv :: BackendEnv
defaultBackendEnv =
    BackendEnv
        { generatedIReg = 0
        , generatedFReg = 0
        , iMap = []
        , fMap = []
        }

type BackendState m = ReaderT BackendConfig (ExceptT CompilerError (StateT BackendEnv (IdentEnvT m)))

genTempIReg :: (Monad m) => BackendState m (Register RegID Int)
genTempIReg = do
    env <- get
    modify $ \e -> e{generatedIReg = generatedIReg e + 1}
    return $ TempReg $ generatedIReg env

genTempFReg :: (Monad m) => BackendState m (Register RegID Float)
genTempFReg = do
    env <- get
    modify $ \e -> e{generatedFReg = generatedFReg e + 1}
    return $ TempReg $ generatedFReg env

genIReg :: (Monad m) => Ident -> BackendState m (Register RegID Int)
genIReg ident = do
    reg <- genTempIReg
    modify $ \e ->
        e
            { iMap = (ident, reg) : iMap e
            }
    pure reg

genFReg :: (Monad m) => Ident -> BackendState m (Register RegID Float)
genFReg ident = do
    reg <- genTempFReg
    modify $ \e ->
        e
            { fMap = (ident, reg) : fMap e
            }
    pure reg

findI :: (Monad m) => Ident -> BackendState m (Register RegID Int)
findI ident = do
    regID <- gets (lookup ident . iMap)
    case regID of
        Just actual -> pure actual
        Nothing -> do
            throwError $ OtherError $ "Detected an unknown identifier named " <> display ident <> "."

findF :: (Monad m) => Ident -> BackendState m (Register RegID Float)
findF ident = do
    regID <- gets (lookup ident . fMap)
    case regID of
        Just actual -> pure actual
        Nothing -> do
            throwError $ OtherError $ "Detected an unknown identifier named " <> display ident <> "."

fromState :: TypedState -> Loc
fromState (TypedState _ pos) = fromSourcePos pos

toInstU :: (Monad m) => ClosureExpr -> BackendState m [Inst Loc RegID AllowBranch]
toInstU (Let{}) =
    throwError $ OtherError "Let itself cannot be regarded as an instruction."
toInstU (If{}) =
    throwError $ OtherError "If itself cannot be regarded as an instruction."
toInstU (Const _ LUnit) = pure []
toInstU (Put state dest idx src) = do
    dest' <- findI dest
    idx' <- findI idx
    srcTy <- lift $ lift $ lift $ getTyOf src
    case srcTy of
        TFloat -> do
            src' <- findF src
            offset <- genTempIReg
            pure
                [ IIntOp (fromState state) Mul offset idx' (Imm 4)
                , IIntOp (fromState state) Add offset dest' (Reg offset)
                , IFStore (fromState state) src' offset 0
                ]
        _ -> do
            src' <- findI src
            offset <- genTempIReg
            pure
                [ IIntOp (fromState state) Mul offset idx' (Imm 4)
                , IIntOp (fromState state) Add offset dest' (Reg offset)
                , IStore (fromState state) src' offset 0
                ]
toInstU (ClosureApp state func args) = do
    func' <- findI func
    args' <- mapM findI args
    argLimit' <- asks argLimit
    when (length args' > argLimit') $ do
        throwError $ OtherError "The number of arguments exceeds the limit."
    pure [IClosureCall (fromState state) func' args' []]
toInstU (DirectApp state func args) = do
    args' <- mapM findI args
    argLimit' <- asks argLimit
    when (length args' > argLimit') $ do
        throwError $ OtherError "The number of arguments exceeds the limit."
    pure [IRichCall (fromState state) (display func) args' []]
toInstU _ =
    throwError $ OtherError "The expression cannot have type unit."

toInstI :: (Monad m) => Register RegID Int -> ClosureExpr -> BackendState m [Inst Loc RegID AllowBranch]
toInstI _ (Let{}) =
    throwError $ OtherError "Let itself cannot be regarded as an instruction."
toInstI _ (If{}) =
    throwError $ OtherError "If itself cannot be regarded as an instruction."
toInstI reg (Const state (LInt i)) =
    pure [IMov (fromState state) reg (Imm i)]
toInstI reg (Const state (LBool b)) =
    pure [IMov (fromState state) reg (Imm $ if b then 1 else 0)]
toInstI reg (Unary state Not operand) = do
    operand' <- findI operand
    pure [ICompOp (fromState state) Eq reg ZeroReg (Reg operand')]
toInstI reg (Unary state Neg operand) = do
    operand' <- findI operand
    pure [IIntOp (fromState state) Sub reg ZeroReg (Reg operand')]
toInstI reg (Binary state (RelationOp op) operand1 operand2) = do
    operand1' <- findI operand1
    operand2' <- findI operand2
    pure [ICompOp (fromState state) op reg operand1' (Reg operand2')]
toInstI reg (Binary state (IntOp op) operand1 operand2) = do
    operand1' <- findI operand1
    operand2' <- findI operand2
    pure [IIntOp (fromState state) op reg operand1' (Reg operand2')]
toInstI reg (Var state ident) = do
    regID <- findI ident
    if Reg reg /= Reg regID
        then
            pure [IMov (fromState state) reg (Reg regID)]
        else
            pure []
toInstI reg (Tuple state vars) = do
    inst <-
        mapM
            ( \(index, var) -> do
                varTy <- lift $ lift $ lift $ getTyOf var
                case varTy of
                    TFloat -> do
                        var' <- findF var
                        pure $ IFStore (fromState state) var' HeapReg (index * 4)
                    _ -> do
                        var' <- findI var
                        pure $ IStore (fromState state) var' HeapReg (index * 4)
            )
            $ zip [0 ..] vars
    pure $
        inst
            ++ [ IMov (fromState state) reg (Reg HeapReg)
               , IIntOp (fromState state) Add HeapReg HeapReg (Imm $ length vars * 4)
               ]
toInstI reg (ArrayCreate state size initVal) = do
    initValTy <- lift $ lift $ lift $ getTyOf initVal
    size' <- findI size
    offset <- genTempIReg
    case initValTy of
        TFloat -> do
            initVal' <- findF initVal
            pure
                [ IFStore (fromState state) initVal' HeapReg 0
                , IIntOp (fromState state) Mul offset size' (Imm 4)
                , IMov (fromState state) reg (Reg HeapReg)
                , IIntOp (fromState state) Add HeapReg HeapReg (Reg offset)
                ]
        _ -> do
            initVal' <- findI initVal
            pure
                [ IStore (fromState state) initVal' HeapReg 0
                , IIntOp (fromState state) Mul offset size' (Imm 4)
                , IMov (fromState state) reg (Reg HeapReg)
                , IIntOp (fromState state) Add HeapReg HeapReg (Reg offset)
                ]
toInstI reg (Get state array index) = do
    array' <- findI array
    index' <- findI index
    offset <- genTempIReg
    addr <- genTempIReg
    pure
        [ IIntOp (fromState state) Mul offset index' (Imm 4)
        , IIntOp (fromState state) Add addr array' (Reg offset)
        , ILoad (fromState state) reg addr 0
        ]
toInstI reg (MakeClosure state func freeV) = do
    freeV' <- mapM findI freeV
    pure [IMakeClosure (fromState state) reg (display func) freeV' []]
toInstI reg (DirectApp state func args) = do
    args' <- mapM findI args
    argLimit' <- asks argLimit
    when (length args' > argLimit') $ do
        throwError $ OtherError "The number of arguments exceeds the limit."
    if reg /= RetReg
        then
            pure
                [ IRichCall (fromState state) (display func) args' []
                , IMov (fromState state) reg (Reg RetReg)
                ]
        else
            pure [IRichCall (fromState state) (display func) args' []]
toInstI reg (ClosureApp state func args) = do
    func' <- findI func
    args' <- mapM findI args
    argLimit' <- asks argLimit
    when (length args' > argLimit') $ do
        throwError $ OtherError "The number of arguments exceeds the limit."
    if reg /= RetReg
        then
            pure
                [ IClosureCall (fromState state) func' args' []
                , IMov (fromState state) reg (Reg RetReg)
                ]
        else
            pure [IClosureCall (fromState state) func' args' []]
toInstI _ _ =
    throwError $ OtherError "The expression cannot be represented as int."

toInstF :: (Monad m) => Register RegID Float -> ClosureExpr -> BackendState m [Inst Loc RegID AllowBranch]
toInstF _ (Let{}) =
    throwError $ OtherError "Let itself cannot be regarded as an instruction."
toInstF _ (If{}) =
    throwError $ OtherError "If itself cannot be regarded as an instruction."
toInstF reg (Const state (LFloat f)) =
    pure [IFMov (fromState state) reg (Imm f)]
toInstF reg (Unary state FNeg operand) = do
    operand' <- findF operand
    pure [IFOp (fromState state) FSub reg ZeroReg operand']
toInstF reg (Binary state (FloatOp op) operand1 operand2) = do
    operand1' <- findF operand1
    operand2' <- findF operand2
    pure [IFOp (fromState state) op reg operand1' operand2']
toInstF reg (Var state ident) = do
    regID <- findF ident
    if reg /= regID
        then
            pure [IFMov (fromState state) reg (Reg regID)]
        else
            pure []
toInstF reg (Get state array index) = do
    array' <- findI array
    index' <- findI index
    offset <- genTempIReg
    addr <- genTempIReg
    pure
        [ IIntOp (fromState state) Mul offset index' (Imm 4)
        , IIntOp (fromState state) Add addr array' (Reg offset)
        , IFLoad (fromState state) reg addr 0
        ]
toInstF reg (DirectApp state func args) = do
    args' <- mapM findI args
    argLimit' <- asks argLimit
    when (length args' > argLimit') $ do
        throwError $ OtherError "The number of arguments exceeds the limit."
    if reg /= RetReg
        then
            pure
                [ IRichCall (fromState state) (display func) args' []
                , IFMov (fromState state) reg (Reg RetReg)
                ]
        else
            pure [IRichCall (fromState state) (display func) args' []]
toInstF _ _ =
    throwError $ OtherError "The expression cannot have type float."

toInst ::
    (Monad m) =>
    Register RegID Int ->
    Register RegID Float ->
    ClosureExpr ->
    BackendState m [Inst Loc RegID AllowBranch]
toInst iReg fReg (If state cond thenExpr elseExpr) = do
    cond' <- findI cond

    thenExpr' <- toInst iReg fReg thenExpr
    elseExpr' <- toInst iReg fReg elseExpr

    pure [IBranch (fromState state) Ne cond' ZeroReg thenExpr' elseExpr']
toInst _ _ (Let _ (PRec _ _) _ _) =
    throwError $ OtherError "The function should be removed during closure conversion."
toInst iReg fReg (Let _ PUnit expr body) = do
    expr' <- toInst iReg fReg expr
    body' <- toInst iReg fReg body
    pure $ expr' ++ body'
toInst iReg fReg (Let _ (PVar v) expr body) = do
    vTy <- lift $ lift $ lift $ getTyOf v
    case vTy of
        TFloat -> do
            v' <- genFReg v
            expr' <- toInst iReg v' expr
            body' <- toInst iReg fReg body
            pure $ expr' ++ body'
        _ -> do
            v' <- genIReg v
            expr' <- toInst v' fReg expr
            body' <- toInst iReg fReg body
            pure $ expr' ++ body'
toInst iReg fReg (Let state (PTuple vals) expr body) = do
    tuple <- genTempIReg
    expr' <- toInst tuple fReg expr
    store <-
        mapM
            ( \(idx, val) -> do
                ty <- lift $ lift $ lift $ getTyOf val
                case ty of
                    TFloat -> do
                        val' <- genFReg val
                        pure $ IFLoad (fromState state) val' tuple (idx * 4)
                    _ -> do
                        val' <- genIReg val
                        pure $ ILoad (fromState state) val' tuple (idx * 4)
            )
            $ zip [0 ..] vals
    body' <- toInst iReg fReg body
    pure $ expr' ++ store ++ body'
toInst iReg fReg expr = do
    case getType (getExprState expr) of
        TUnit -> do
            toInstU expr
        TFloat -> do
            toInstF fReg expr
        _ -> do
            toInstI iReg expr

toInstructions :: (Monad m) => BackendConfig -> Function -> IdentEnvT m (Either CompilerError (IntermediateCodeBlock Loc RegID))
toInstructions config function = do
    result <-
        runStateT
            ( runExceptT $ runReaderT (toInstructions' function) config
            )
            defaultBackendEnv
    case result of
        (Left err, _) -> pure $ Left err
        (Right inst, _) -> pure $ Right $ IntermediateCodeBlock (display (funcName function)) inst
  where
    toInstructions' :: (Monad m) => Function -> BackendState m [Inst Loc RegID AllowBranch]
    toInstructions' (Function _ _ _ freeVars' boundedArgs' body) = do
        modify $ \env ->
            env
                { generatedIReg = 0
                , generatedFReg = 0
                , iMap = argReg
                , fMap = []
                }
        toInst RetReg RetReg body
      where
        freeReg = zipWith (\v i -> (v, ArgsReg i)) freeVars' [0 ..]
        boundedReg =
            zipWith
                (\v i -> (v, ArgsReg i))
                boundedArgs'
                [(length freeReg) ..]
        argReg = freeReg ++ boundedReg
