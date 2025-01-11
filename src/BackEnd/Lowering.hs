{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module BackEnd.Lowering (
    RegID,
    BackendIdentState,
    toInstructions,
) where

import BackEnd.BackendEnv
import BackEnd.FunctionCall (saveArgs)
import Builtin (BuiltinFunction (builtinInst), builtinMakeTuple, findBuiltin)
import Control.Monad (filterM)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (modify)
import Data.Map (fromList)
import Data.Text (Text)
import Display (display)
import Error (CompilerError (OtherError))
import IR
import MiddleEnd.Analysis.Identifier (IdentEnvT, asConstant, getTyOf)
import MiddleEnd.Globals (GlobalProp (globalOffset))
import Registers (
    RegOrImm (Imm, Reg),
    RegType (RFloat, RInt),
    Register,
    argsReg,
    heapReg,
    retReg,
    updateVariant,
    zeroReg,
 )
import Syntax
import Typing (TypeKind (TFloat, TUnit))

type BackendIdentState m = BackendStateT (IdentEnvT m)

resolveArgs :: (Monad m) => [Ident] -> BackendIdentState m ([RegOrImm RegID Int], [Register RegID Float])
resolveArgs args = do
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
    -- TODO: Supports global variables.
    iArgs' <- mapM (findRegOrImm RInt) iArgs
    fArgs' <- mapM (findReg RFloat) fArgs
    pure (iArgs', fArgs')

expandExprToInst ::
    (Monad m) =>
    Register RegID Int ->
    Register RegID Float ->
    ClosureExpr ->
    BackendIdentState m [Inst Loc RegID AllowBranch]
expandExprToInst _ _ (Const _ LUnit) = do
    pure []
expandExprToInst iReg _ (Const state (LInt i)) = do
    pure [IMov (getLoc state) iReg (Imm i)]
expandExprToInst iReg _ (Const state (LBool b)) = do
    pure [IMov (getLoc state) iReg (Imm $ if b then 1 else 0)]
expandExprToInst _ fReg (Const state (LFloat f)) = do
    pure [IFMov (getLoc state) fReg (Imm f)]
expandExprToInst iReg _ (Unary state Not operand) = do
    operand' <- findReg RInt operand
    pure [ICompOp (getLoc state) Eq iReg (zeroReg RInt) (Reg operand')]
expandExprToInst iReg _ (Unary state Neg operand) = do
    operand' <- findReg RInt operand
    pure [IIntOp (getLoc state) PSub iReg (zeroReg RInt) (Reg operand')]
expandExprToInst _ fReg (Unary state FNeg operand) = do
    operand' <- findReg RFloat operand
    pure [IFOp (getLoc state) FSub fReg (zeroReg RFloat) operand']
expandExprToInst iReg _ (Binary state (RelationOp op) operand1 operand2) = do
    ty <- liftB $ getTyOf operand1
    case ty of
        TFloat -> do
            operand1' <- findReg RFloat operand1
            operand2' <- findReg RFloat operand2
            pure [IFCompOp (getLoc state) op iReg operand1' operand2']
        _ -> do
            operand1' <- findReg RInt operand1
            operand2' <- findReg RInt operand2
            pure [ICompOp (getLoc state) op iReg operand1' (Reg operand2')]
expandExprToInst iReg _ (Binary state (IntOp op) operand1 operand2) = do
    const2 <- liftB $ asConstant operand2
    case (const2, op) of
        (Just (LInt i), Sub) -> do
            operand1' <- findReg RInt operand1
            pure [IIntOp (getLoc state) PAdd iReg operand1' (Imm $ -i)]
        (Just (LInt i), _) -> do
            operand1' <- findReg RInt operand1
            pure [IIntOp (getLoc state) (fromIntBinOp op) iReg operand1' (Imm i)]
        _ -> do
            operand1' <- findReg RInt operand1
            operand2' <- findReg RInt operand2
            pure [IIntOp (getLoc state) (fromIntBinOp op) iReg operand1' (Reg operand2')]
expandExprToInst _ fReg (Binary state (FloatOp op) operand1 operand2) = do
    operand1' <- findReg RFloat operand1
    operand2' <- findReg RFloat operand2
    pure [IFOp (getLoc state) op fReg operand1' operand2']
expandExprToInst iReg fReg (If state (CIdentity cond) thenExpr elseExpr) = do
    cond' <- findReg RInt cond

    thenExpr' <- expandExprToInst iReg fReg thenExpr
    elseExpr' <- expandExprToInst iReg fReg elseExpr

    pure [IBranch (getLoc state) Ne cond' (zeroReg RInt) thenExpr' elseExpr']
expandExprToInst iReg fReg (If state (CComp op lhs rhs) thenExpr elseExpr) = do
    thenExpr' <- expandExprToInst iReg fReg thenExpr
    elseExpr' <- expandExprToInst iReg fReg elseExpr

    lhsTy <- liftB $ getTyOf lhs
    case lhsTy of
        TFloat -> do
            lhs' <- findReg RFloat lhs
            rhs' <- findReg RFloat rhs
            cond <- genTempReg RInt

            pure
                [ IFCompOp (getLoc state) op cond lhs' rhs'
                , IBranch (getLoc state) Ne cond (zeroReg RInt) thenExpr' elseExpr'
                ]
        _ -> do
            lhs' <- findReg RInt lhs
            rhs' <- findReg RInt rhs

            pure [IBranch (getLoc state) op lhs' rhs' thenExpr' elseExpr']
expandExprToInst _ _ (Let _ (PRec _ _) _ _) =
    throwError $ OtherError "The function should be removed during closure conversion."
expandExprToInst iReg fReg (Let _ PUnit expr body) = do
    expr' <- expandExprToInst iReg fReg expr
    body' <- expandExprToInst iReg fReg body
    pure $ expr' ++ body'
expandExprToInst iReg fReg (Let _ (PVar v) expr body) = do
    vTy <- liftB $ getTyOf v
    case vTy of
        TFloat -> do
            v' <- genReg RFloat v
            expr' <- expandExprToInst iReg v' expr
            body' <- expandExprToInst iReg fReg body
            pure $ expr' ++ body'
        _ -> do
            v' <- genReg RInt v
            expr' <- expandExprToInst v' fReg expr
            body' <- expandExprToInst iReg fReg body
            pure $ expr' ++ body'
expandExprToInst iReg fReg (Let state (PTuple vals) expr body) = do
    tuple <- genTempReg RInt
    expr' <- expandExprToInst tuple fReg expr
    store <-
        mapM
            ( \(idx, val) -> do
                ty <- liftB $ getTyOf val
                case ty of
                    TFloat -> do
                        val' <- genReg RFloat val
                        pure $ IFLoad (getLoc state) val' tuple idx
                    _ -> do
                        val' <- genReg RInt val
                        pure $ ILoad (getLoc state) val' tuple idx
            )
            $ zip [0 ..] vals
    body' <- expandExprToInst iReg fReg body
    pure $ expr' ++ store ++ body'
expandExprToInst iReg _ (Var state (ExternalIdent ext)) = do
    -- Possibly a global variable.
    prop <- findGlobal ext
    let offset = globalOffset prop
    pure [IMov (getLoc state) iReg (Imm offset)]
expandExprToInst iReg fReg (Var state ident) = do
    ty <- liftB $ getTyOf ident
    case ty of
        TUnit -> do
            pure []
        TFloat -> do
            regID <- findReg RFloat ident
            if fReg /= regID
                then pure [IFMov (getLoc state) fReg (Reg regID)]
                else pure []
        _ -> do
            regID <- findReg RInt ident
            if iReg /= regID
                then pure [IMov (getLoc state) iReg (Reg regID)]
                else pure []
expandExprToInst iReg _ (Tuple state vars) = do
    inst <-
        concat
            <$> mapM
                ( \(index, var) -> do
                    case var of
                        ExternalIdent ext -> do
                            -- An element of the tuple can be a global variable.
                            temp <- genTempReg RInt
                            prop <- findGlobal ext
                            let offset = globalOffset prop
                            pure
                                [ IMov (getLoc state) temp (Imm offset)
                                , IStore (getLoc state) temp heapReg index
                                ]
                        _ -> do
                            varTy <- liftB $ getTyOf var
                            case varTy of
                                TFloat -> do
                                    var' <- findReg RFloat var
                                    pure [IFStore (getLoc state) var' heapReg index]
                                _ -> do
                                    var' <- findReg RInt var
                                    pure [IStore (getLoc state) var' heapReg index]
                )
                (zip [0 ..] vars)
    pure $
        inst
            ++ [ IMov (getLoc state) iReg (Reg heapReg)
               , IIntOp (getLoc state) PAdd heapReg heapReg (Imm $ length vars)
               ]
expandExprToInst iReg _ (ArrayCreate state size _) = do
    size' <- findReg RInt size
    pure
        [ IMov (getLoc state) iReg (Reg heapReg)
        , IIntOp (getLoc state) PAdd heapReg heapReg (Reg size')
        ]
expandExprToInst iReg fReg (Get state array index) = do
    array' <- case array of
        ExternalIdent arrayName ->
            Imm . globalOffset <$> findGlobal arrayName
        _ -> Reg <$> findReg RInt array
    index' <- findReg RInt index
    addr <- genTempReg RInt
    let ty = getType state
    case ty of
        TFloat ->
            pure
                [ IIntOp (getLoc state) PAdd addr index' array'
                , IFLoad (getLoc state) fReg addr 0
                ]
        _ ->
            pure
                [ IIntOp (getLoc state) PAdd addr index' array'
                , ILoad (getLoc state) iReg addr 0
                ]
expandExprToInst _ _ (Put state dest idx src) = do
    dest' <- findRegOrImm RInt dest
    idx' <- findReg RInt idx
    srcTy <- liftB $ getTyOf src
    case src of
        ExternalIdent src' -> do
            -- It can be a global variable.
            src'' <- globalOffset <$> findGlobal src'
            temp <- genTempReg RInt
            offset <- genTempReg RInt
            pure
                [ IIntOp (getLoc state) PAdd offset idx' dest'
                , IMov (getLoc state) temp (Imm src'')
                , IStore (getLoc state) temp offset 0
                ]
        _ ->
            case srcTy of
                TFloat -> do
                    src' <- findReg RFloat src
                    offset <- genTempReg RInt
                    pure
                        [ IIntOp (getLoc state) PAdd offset idx' dest'
                        , IFStore (getLoc state) src' offset 0
                        ]
                _ -> do
                    src' <- findReg RInt src
                    offset <- genTempReg RInt
                    pure
                        [ IIntOp (getLoc state) PAdd offset idx' dest'
                        , IStore (getLoc state) src' offset 0
                        ]
expandExprToInst iReg _ (MakeClosure state func freeV) = do
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
    -- Need to proof iFreeV and fFreeV don't contain any global variables.
    iFreeV' <- mapM (findRegOrImm RInt) iFreeV
    fFreeV' <- mapM (findReg RFloat) fFreeV
    pure [IMakeClosure (getLoc state) iReg (display func) iFreeV' fFreeV']
expandExprToInst iReg fReg (DirectApp state func args) = do
    case args of
        (ExternalIdent tuple) : values -> do
            if func == builtinMakeTuple
                then
                    mkTuple tuple values
                else
                    simpleApp
        _ -> simpleApp
  where
    mkTuple :: (Monad m) => Text -> [Ident] -> BackendIdentState m [Inst Loc RegID AllowBranch]
    mkTuple tuple values = do
        prop <- findGlobal tuple
        let offset = globalOffset prop
        concat
            <$> mapM
                ( \(idx, val) -> do
                    ty <- liftB $ getTyOf val
                    case ty of
                        TFloat -> do
                            reg <- findReg RFloat val
                            pure [IFStore (getLoc state) reg (zeroReg RInt) (offset + idx)]
                        _ -> do
                            reg <- findRegOrImm RInt val
                            case reg of
                                Reg reg' -> do
                                    pure [IStore (getLoc state) reg' (zeroReg RInt) (offset + idx)]
                                Imm imm -> do
                                    temp' <- genTempReg RInt
                                    pure
                                        [ IMov (getLoc state) temp' (Imm imm)
                                        , IStore (getLoc state) temp' (zeroReg RInt) (offset + idx)
                                        ]
                )
                ( zip
                    [0 ..]
                    values
                )

    simpleApp ::
        (Monad m) =>
        BackendIdentState m [Inst Loc RegID AllowBranch]
    simpleApp = do
        (iArgs', fArgs') <- resolveArgs args
        case getType state of
            TUnit -> do
                case findBuiltin func of
                    Just builtin -> do
                        pure [IRawInst (getLoc state) (builtinInst builtin) RIRUnit iArgs' fArgs']
                    Nothing -> do
                        pure [IRichCall (getLoc state) (display func) iArgs' fArgs']
            TFloat -> do
                case findBuiltin func of
                    Just builtin -> do
                        pure [IRawInst (getLoc state) (builtinInst builtin) (RIRFloat fReg) iArgs' fArgs']
                    Nothing -> do
                        pure
                            [ IRichCall (getLoc state) (display func) iArgs' fArgs'
                            , IFMov (getLoc state) fReg (Reg $ retReg RFloat)
                            ]
            _ -> do
                case findBuiltin func of
                    Just builtin -> do
                        pure [IRawInst (getLoc state) (builtinInst builtin) (RIRInt iReg) iArgs' fArgs']
                    Nothing -> do
                        pure
                            [ IRichCall (getLoc state) (display func) iArgs' fArgs'
                            , IMov (getLoc state) iReg (Reg $ retReg RInt)
                            ]
expandExprToInst iReg fReg (ClosureApp state func args) = do
    func' <- findReg RInt func
    (iArgs', fArgs') <- resolveArgs args
    case getType state of
        TUnit -> do
            pure [IClosureCall (getLoc state) func' iArgs' fArgs']
        TFloat -> do
            pure
                [ IClosureCall (getLoc state) func' iArgs' fArgs'
                , IFMov (getLoc state) fReg (Reg $ retReg RFloat)
                ]
        _ -> do
            pure
                [ IClosureCall (getLoc state) func' iArgs' fArgs'
                , IMov (getLoc state) iReg (Reg $ retReg RInt)
                ]

toInstructions :: (Monad m) => Function -> BackendIdentState m (IntermediateCodeBlock Loc RegID)
toInstructions function = do
    inst <- toInstructions' function
    saveArgs (IntermediateCodeBlock (display $ funcName function) 0 inst)
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
                    (\v i -> (v, argsReg RInt i))
                    iBoundedArgs
                    [0 ..]
        let fBoundedReg =
                zipWith
                    (\v i -> (v, argsReg RFloat i))
                    fBoundedArgs
                    [0 ..]
        let closureArg = argsReg RInt $ length iBoundedReg
        iFreeVarsReg <-
            mapM
                ( \(v, i) -> do
                    reg <- genReg RInt v
                    pure (ILoad dummyLoc reg closureArg i, (v, reg))
                )
                $ zip iFreeVars [0 ..]
        fFreeVarsReg <-
            mapM
                ( \(v, i) -> do
                    reg <- genReg RFloat v
                    pure (IFLoad dummyLoc reg closureArg i, (v, reg))
                )
                $ zip fFreeVars [length iFreeVars ..]

        modify $ \env ->
            env
                { regContext =
                    updateVariant
                        RInt
                        ( \ctx ->
                            ctx
                                { argsLength = length iBoundedReg
                                , registerMap = fromList $ iBoundedReg ++ map snd iFreeVarsReg
                                }
                        )
                        $ regContext env
                }
        modify $ \env ->
            env
                { regContext =
                    updateVariant
                        RFloat
                        ( \ctx ->
                            ctx
                                { argsLength = length fBoundedReg
                                , registerMap = fromList $ fBoundedReg ++ map snd fFreeVarsReg
                                }
                        )
                        $ regContext env
                }
        inst <- expandExprToInst (retReg RInt) (retReg RFloat) body
        pure $ map fst iFreeVarsReg ++ map fst fFreeVarsReg ++ inst
