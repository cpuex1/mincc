{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.Lowering (
    RegID,
    BackendIdentState,
    toInstructions,
) where

import Backend.Asm
import Backend.BackendEnv
import Backend.FunctionCall (saveArgs, saveRegisters, saveReturnAddress)
import Control.Monad (filterM)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (modify)
import Display (display)
import Error (CompilerError (OtherError))
import IdentAnalysis (IdentEnvT, getTyOf)
import Syntax
import Typing (TypeKind (TFloat, TUnit))

type BackendIdentState m = BackendStateT (IdentEnvT m)

toInstU :: (Monad m) => ClosureExpr -> BackendIdentState m [Inst Loc RegID AllowBranch]
toInstU (Let{}) =
    throwError $ OtherError "Let itself cannot be regarded as an instruction."
toInstU (If{}) =
    throwError $ OtherError "If itself cannot be regarded as an instruction."
toInstU (Const _ LUnit) = pure []
toInstU (Put state dest idx src) = do
    dest' <- findI dest
    idx' <- findI idx
    srcTy <- liftB $ getTyOf src
    case srcTy of
        TFloat -> do
            src' <- findF src
            offset <- genTempIReg
            pure
                [ IIntOp (getLoc state) Mul offset idx' (Imm 4)
                , IIntOp (getLoc state) Add offset dest' (Reg offset)
                , IFStore (getLoc state) src' offset 0
                ]
        _ -> do
            src' <- findI src
            offset <- genTempIReg
            pure
                [ IIntOp (getLoc state) Mul offset idx' (Imm 4)
                , IIntOp (getLoc state) Add offset dest' (Reg offset)
                , IStore (getLoc state) src' offset 0
                ]
toInstU (ClosureApp state func args) = do
    func' <- findI func
    iArgs <-
        filterM
            ( \arg -> do
                ty <- liftB $ getTyOf arg
                pure $ ty /= TFloat
            )
            args
    fArgs <-
        filterM
            ( \arg -> do
                ty <- liftB $ getTyOf arg
                pure $ ty == TFloat
            )
            args
    iArgs' <- mapM findI iArgs
    fArgs' <- mapM findF fArgs
    pure [IClosureCall (getLoc state) func' iArgs' fArgs']
toInstU (DirectApp state func args) = do
    iArgs <-
        filterM
            ( \arg -> do
                ty <- liftB $ getTyOf arg
                pure $ ty /= TFloat
            )
            args
    fArgs <-
        filterM
            ( \arg -> do
                ty <- liftB $ getTyOf arg
                pure $ ty == TFloat
            )
            args
    iArgs' <- mapM findI iArgs
    fArgs' <- mapM findF fArgs
    pure [IRichCall (getLoc state) (display func) iArgs' fArgs']
toInstU _ =
    throwError $ OtherError "The expression cannot have type unit."

toInstI :: (Monad m) => Register RegID Int -> ClosureExpr -> BackendIdentState m [Inst Loc RegID AllowBranch]
toInstI _ (Let{}) =
    throwError $ OtherError "Let itself cannot be regarded as an instruction."
toInstI _ (If{}) =
    throwError $ OtherError "If itself cannot be regarded as an instruction."
toInstI reg (Const state (LInt i)) =
    pure [IMov (getLoc state) reg (Imm i)]
toInstI reg (Const state (LBool b)) =
    pure [IMov (getLoc state) reg (Imm $ if b then 1 else 0)]
toInstI reg (Unary state Not operand) = do
    operand' <- findI operand
    pure [ICompOp (getLoc state) Eq reg ZeroReg (Reg operand')]
toInstI reg (Unary state Neg operand) = do
    operand' <- findI operand
    pure [IIntOp (getLoc state) Sub reg ZeroReg (Reg operand')]
toInstI reg (Binary state (RelationOp op) operand1 operand2) = do
    t <- liftB $ getTyOf operand1
    case t of
        TFloat -> do
            operand1' <- findF operand1
            operand2' <- findF operand2
            pure [IFCompOp (getLoc state) op reg operand1' operand2']
        _ -> do
            operand1' <- findI operand1
            operand2' <- findI operand2
            pure [ICompOp (getLoc state) op reg operand1' (Reg operand2')]
toInstI reg (Binary state (IntOp op) operand1 operand2) = do
    operand1' <- findI operand1
    operand2' <- findI operand2
    pure [IIntOp (getLoc state) op reg operand1' (Reg operand2')]
toInstI reg (Var state ident) = do
    regID <- findI ident
    if Reg reg /= Reg regID
        then
            pure [IMov (getLoc state) reg (Reg regID)]
        else
            pure []
toInstI reg (Tuple state vars) = do
    inst <-
        mapM
            ( \(index, var) -> do
                varTy <- liftB $ getTyOf var
                case varTy of
                    TFloat -> do
                        var' <- findF var
                        pure $ IFStore (getLoc state) var' HeapReg (index * 4)
                    _ -> do
                        var' <- findI var
                        pure $ IStore (getLoc state) var' HeapReg (index * 4)
            )
            $ zip [0 ..] vars
    pure $
        inst
            ++ [ IMov (getLoc state) reg (Reg HeapReg)
               , IIntOp (getLoc state) Add HeapReg HeapReg (Imm $ length vars * 4)
               ]
toInstI reg (ArrayCreate state size initVal) = do
    initValTy <- liftB $ getTyOf initVal
    size' <- findI size
    offset <- genTempIReg
    case initValTy of
        TFloat -> do
            initVal' <- findF initVal
            pure
                [ IFStore (getLoc state) initVal' HeapReg 0
                , IIntOp (getLoc state) Mul offset size' (Imm 4)
                , IMov (getLoc state) reg (Reg HeapReg)
                , IIntOp (getLoc state) Add HeapReg HeapReg (Reg offset)
                ]
        _ -> do
            initVal' <- findI initVal
            pure
                [ IStore (getLoc state) initVal' HeapReg 0
                , IIntOp (getLoc state) Mul offset size' (Imm 4)
                , IMov (getLoc state) reg (Reg HeapReg)
                , IIntOp (getLoc state) Add HeapReg HeapReg (Reg offset)
                ]
toInstI reg (Get state array index) = do
    array' <- findI array
    index' <- findI index
    offset <- genTempIReg
    addr <- genTempIReg
    pure
        [ IIntOp (getLoc state) Mul offset index' (Imm 4)
        , IIntOp (getLoc state) Add addr array' (Reg offset)
        , ILoad (getLoc state) reg addr 0
        ]
toInstI reg (MakeClosure state func freeV) = do
    iFreeV <-
        filterM
            ( \arg -> do
                ty <- liftB $ getTyOf arg
                pure $ ty /= TFloat
            )
            freeV
    fFreeV <-
        filterM
            ( \arg -> do
                ty <- liftB $ getTyOf arg
                pure $ ty == TFloat
            )
            freeV
    iFreeV' <- mapM findI iFreeV
    fFreeV' <- mapM findF fFreeV
    pure [IMakeClosure (getLoc state) reg (display func) iFreeV' fFreeV']
toInstI reg (DirectApp state func args) = do
    iArgs <-
        filterM
            ( \arg -> do
                ty <- liftB $ getTyOf arg
                pure $ ty /= TFloat
            )
            args
    fArgs <-
        filterM
            ( \arg -> do
                ty <- liftB $ getTyOf arg
                pure $ ty == TFloat
            )
            args
    iArgs' <- mapM findI iArgs
    fArgs' <- mapM findF fArgs
    if reg /= RetReg
        then
            pure
                [ IRichCall (getLoc state) (display func) iArgs' fArgs'
                , IMov (getLoc state) reg (Reg RetReg)
                ]
        else
            pure [IRichCall (getLoc state) (display func) iArgs' fArgs']
toInstI reg (ClosureApp state func args) = do
    func' <- findI func
    iArgs <-
        filterM
            ( \arg -> do
                ty <- liftB $ getTyOf arg
                pure $ ty /= TFloat
            )
            args
    fArgs <-
        filterM
            ( \arg -> do
                ty <- liftB $ getTyOf arg
                pure $ ty == TFloat
            )
            args
    iArgs' <- mapM findI iArgs
    fArgs' <- mapM findF fArgs
    if reg /= RetReg
        then
            pure
                [ IClosureCall (getLoc state) func' iArgs' fArgs'
                , IMov (getLoc state) reg (Reg RetReg)
                ]
        else
            pure [IClosureCall (getLoc state) func' iArgs' fArgs']
toInstI _ _ =
    throwError $ OtherError "The expression cannot be represented as int."

toInstF :: (Monad m) => Register RegID Float -> ClosureExpr -> BackendIdentState m [Inst Loc RegID AllowBranch]
toInstF _ (Let{}) =
    throwError $ OtherError "Let itself cannot be regarded as an instruction."
toInstF _ (If{}) =
    throwError $ OtherError "If itself cannot be regarded as an instruction."
toInstF reg (Const state (LFloat f)) =
    pure [IFMov (getLoc state) reg (Imm f)]
toInstF reg (Unary state FNeg operand) = do
    operand' <- findF operand
    pure [IFOp (getLoc state) FSub reg ZeroReg operand']
toInstF reg (Binary state (FloatOp op) operand1 operand2) = do
    operand1' <- findF operand1
    operand2' <- findF operand2
    pure [IFOp (getLoc state) op reg operand1' operand2']
toInstF reg (Var state ident) = do
    regID <- findF ident
    if reg /= regID
        then
            pure [IFMov (getLoc state) reg (Reg regID)]
        else
            pure []
toInstF reg (Get state array index) = do
    array' <- findI array
    index' <- findI index
    offset <- genTempIReg
    addr <- genTempIReg
    pure
        [ IIntOp (getLoc state) Mul offset index' (Imm 4)
        , IIntOp (getLoc state) Add addr array' (Reg offset)
        , IFLoad (getLoc state) reg addr 0
        ]
toInstF reg (DirectApp state func args) = do
    iArgs <-
        filterM
            ( \arg -> do
                ty <- liftB $ getTyOf arg
                pure $ ty /= TFloat
            )
            args
    fArgs <-
        filterM
            ( \arg -> do
                ty <- liftB $ getTyOf arg
                pure $ ty == TFloat
            )
            args
    iArgs' <- mapM findI iArgs
    fArgs' <- mapM findF fArgs
    if reg /= RetReg
        then
            pure
                [ IRichCall (getLoc state) (display func) iArgs' fArgs'
                , IFMov (getLoc state) reg (Reg RetReg)
                ]
        else
            pure [IRichCall (getLoc state) (display func) iArgs' fArgs']
toInstF reg (ClosureApp state func args) = do
    func' <- findI func
    iArgs <-
        filterM
            ( \arg -> do
                ty <- liftB $ getTyOf arg
                pure $ ty /= TFloat
            )
            args
    fArgs <-
        filterM
            ( \arg -> do
                ty <- liftB $ getTyOf arg
                pure $ ty == TFloat
            )
            args
    iArgs' <- mapM findI iArgs
    fArgs' <- mapM findF fArgs
    if reg /= RetReg
        then
            pure
                [ IClosureCall (getLoc state) func' iArgs' fArgs'
                , IFMov (getLoc state) reg (Reg RetReg)
                ]
        else
            pure [IClosureCall (getLoc state) func' iArgs' fArgs']
toInstF _ _ =
    throwError $ OtherError "The expression cannot have type float."

toInst ::
    (Monad m) =>
    Register RegID Int ->
    Register RegID Float ->
    ClosureExpr ->
    BackendIdentState m [Inst Loc RegID AllowBranch]
toInst iReg fReg (If state cond thenExpr elseExpr) = do
    cond' <- findI cond

    thenExpr' <- toInst iReg fReg thenExpr
    elseExpr' <- toInst iReg fReg elseExpr

    pure [IBranch (getLoc state) Ne cond' ZeroReg thenExpr' elseExpr']
toInst _ _ (Let _ (PRec _ _) _ _) =
    throwError $ OtherError "The function should be removed during closure conversion."
toInst iReg fReg (Let _ PUnit expr body) = do
    expr' <- toInst iReg fReg expr
    body' <- toInst iReg fReg body
    pure $ expr' ++ body'
toInst iReg fReg (Let _ (PVar v) expr body) = do
    vTy <- liftB $ getTyOf v
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
                ty <- liftB $ getTyOf val
                case ty of
                    TFloat -> do
                        val' <- genFReg val
                        pure $ IFLoad (getLoc state) val' tuple (idx * 4)
                    _ -> do
                        val' <- genIReg val
                        pure $ ILoad (getLoc state) val' tuple (idx * 4)
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

toInstructions :: (Monad m) => Function -> BackendIdentState m (IntermediateCodeBlock Loc RegID)
toInstructions function = do
    inst <- toInstructions' function
    inst' <- saveArgs (IntermediateCodeBlock (display $ funcName function) [] inst [])
    pure $ saveReturnAddress $ saveRegisters inst'
  where
    toInstructions' :: (Monad m) => Function -> BackendIdentState m [Inst Loc RegID AllowBranch]
    toInstructions' (Function _ _ _ freeVars' boundedArgs' body) = do
        iBoundedArgs <-
            filterM
                ( \v -> do
                    ty <- liftB $ getTyOf v
                    pure $ ty /= TFloat
                )
                boundedArgs'
        fBoundedArgs <-
            filterM
                ( \v -> do
                    ty <- liftB $ getTyOf v
                    pure $ ty == TFloat
                )
                boundedArgs'
        iFreeVars <-
            filterM
                ( \v -> do
                    ty <- liftB $ getTyOf v
                    pure $ ty /= TFloat
                )
                freeVars'
        fFreeVars <-
            filterM
                ( \v -> do
                    ty <- liftB $ getTyOf v
                    pure $ ty == TFloat
                )
                freeVars'

        let iBoundedReg =
                zipWith
                    (\v i -> (v, ArgsReg i))
                    iBoundedArgs
                    [0 ..]
        let fBoundedReg =
                zipWith
                    (\v i -> (v, ArgsReg i))
                    fBoundedArgs
                    [0 ..]
        let closureArg = ArgsReg $ length iBoundedReg
        iFreeVarsReg <-
            mapM
                ( \(v, i) -> do
                    reg <- genIReg v
                    pure (ILoad dummyLoc reg closureArg (i * 4), (v, reg))
                )
                $ zip iFreeVars [0 ..]
        fFreeVarsReg <-
            mapM
                ( \(v, i) -> do
                    reg <- genFReg v
                    pure (IFLoad dummyLoc reg closureArg (i * 4), (v, reg))
                )
                $ zip fFreeVars [length iFreeVars ..]

        modify $ \env ->
            env
                { iArgsLen = length iBoundedReg
                , fArgsLen = length fBoundedReg
                , iMap = iBoundedReg ++ map snd iFreeVarsReg
                , fMap = fBoundedReg ++ map snd fFreeVarsReg
                }
        inst <- toInst RetReg RetReg body
        pure $ map fst iFreeVarsReg ++ map fst fFreeVarsReg ++ inst
