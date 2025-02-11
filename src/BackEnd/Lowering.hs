{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module BackEnd.Lowering (
    RegID,
    BackendIdentState,
    toInstructions,
    generateBlockGraph,
) where

import BackEnd.BackendEnv
import Builtin (BuiltinFunction (builtinInst), builtinMakeTuple, findBuiltin)
import CodeBlock (
    BlockGraph (BlockGraph),
    CodeBlock (CodeBlock),
    Terminator (TBranch, TJmp, TReturn),
 )
import Control.Monad (filterM, when)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (MonadTrans (lift), StateT, execStateT, gets, modify)
import Data.Foldable (Foldable (toList))
import Data.Sequence (Seq, fromList, (|>))
import Data.Text (Text)
import Display (display)
import Error (CompilerError (OtherError))
import IR
import MiddleEnd.Analysis.Identifier (IdentEnvT, asConstant, getTyOf)
import MiddleEnd.Globals (GlobalProp (globalOffset))
import Registers (
    RegID,
    RegOrImm (Imm, Reg),
    RegType (RFloat, RInt),
    Register (Register),
    RegisterKind (DCReg),
    argsReg,
    heapReg,
    retReg,
    withRegType,
    zeroReg,
    (#!!),
 )
import Syntax (
    BinaryOp (FloatOp, IntOp, RelationOp),
    ClosureExpr,
    Cond (CComp, CIdentity),
    Expr (
        ArrayCreate,
        Binary,
        ClosureApp,
        Const,
        Continue,
        DirectApp,
        Get,
        If,
        Let,
        Loop,
        MakeClosure,
        Put,
        Tuple,
        Unary,
        Var
    ),
    FloatBinOp (FSub),
    Function (Function, funcName),
    Ident (ExternalIdent),
    IntBinOp (Sub),
    Literal (LBool, LFloat, LInt, LUnit),
    Pattern (PRec, PTuple, PUnit, PVar),
    RelationBinOp (Eq, Ne),
    TypedState (getLoc, getType),
    UnaryOp (FNeg, Neg, Not),
    dummyLoc,
 )
import Typing (TypeKind (TFloat, TInt, TUnit))

type BackendIdentState m = BackendStateT (IdentEnvT m)

filterVars :: (Monad m) => RegType rTy -> [Ident] -> BackendIdentState m [Ident]
filterVars RFloat vars = do
    filterM
        ( \v -> do
            ty' <- liftB $ getTyOf v
            pure $ ty' == TFloat
        )
        vars
filterVars RInt vars = do
    filterM
        ( \v -> do
            ty' <- liftB $ getTyOf v
            pure $ ty' /= TFloat
        )
        vars
genRegisters :: (Monad m) => RegType rTy -> [Ident] -> BackendIdentState m [Register RegID rTy]
genRegisters rTy vars = do
    filtered <- filterVars rTy vars
    mapM (genReg rTy) filtered

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

data CodeBlockContext = CodeBlockContext
    { currentLabel :: InstLabel
    , instBuf :: Seq AbstInst
    , generatedBlocks :: [CodeBlock AbstInstKind]
    , contextName :: Text
    , generatedID :: Int
    , prevLabels :: [InstLabel]
    , loopLabel :: InstLabel
    , iLoopArgs :: [Register RegID Int]
    , fLoopArgs :: [Register RegID Float]
    }

type CodeBlockStateT m = StateT CodeBlockContext (BackendIdentState m)

addInst :: (Monad m) => AbstInst -> CodeBlockStateT m ()
addInst inst =
    modify $ \ctx -> ctx{instBuf = instBuf ctx |> inst}

appendInst :: (Monad m) => [AbstInst] -> CodeBlockStateT m ()
appendInst insts =
    modify $ \ctx -> ctx{instBuf = instBuf ctx <> fromList insts}

genNewBlock :: (Monad m) => InstLabel -> [InstLabel] -> CodeBlockStateT m ()
genNewBlock label prevLabels' = do
    modify $ \ctx ->
        ctx
            { currentLabel = label
            , prevLabels = prevLabels'
            , instBuf = mempty
            }

terminate :: (Monad m) => Terminator AbstInstKind -> CodeBlockStateT m ()
terminate term = do
    modify $ \ctx ->
        ctx
            { generatedBlocks =
                generatedBlocks ctx
                    ++ [CodeBlock (currentLabel ctx) (toList $ instBuf ctx) (prevLabels ctx) term]
            }

genNewLabelId :: (Monad m) => CodeBlockStateT m Text
genNewLabelId = do
    labelID <- gets generatedID
    modify $ \ctx' -> ctx'{generatedID = labelID + 1}
    pure $ display labelID

resolveRegOrImm :: (Monad m) => RegOrImm RegID rTy -> CodeBlockStateT m (Register RegID rTy)
resolveRegOrImm (Reg reg) = pure reg
resolveRegOrImm (Imm ty imm) = do
    temp <- lift $ genTempReg ty
    addInst $ IMov dummyLoc temp (Imm ty imm)
    pure temp

takeReg :: (Monad m) => RegType rTy -> Ident -> CodeBlockStateT m (Register RegID rTy)
takeReg rTy ident = lift (findRegOrImm rTy ident) >>= resolveRegOrImm

generateInstructions ::
    (Monad m) =>
    Register RegID Int ->
    Register RegID Float ->
    ClosureExpr ->
    CodeBlockStateT m ()
generateInstructions _ _ (Const _ LUnit) =
    pure ()
generateInstructions iReg _ (Const state (LInt i)) =
    addInst $ IMov (getLoc state) iReg (Imm RInt i)
generateInstructions iReg _ (Const state (LBool b)) =
    addInst $ IMov (getLoc state) iReg (Imm RInt $ if b then 1 else 0)
generateInstructions _ fReg (Const state (LFloat f)) =
    addInst $ IMov (getLoc state) fReg (Imm RFloat f)
generateInstructions iReg _ (Unary state Not operand) = do
    operand' <- lift $ findReg RInt operand
    addInst $ ICompOp (getLoc state) Eq iReg (zeroReg RInt) (Reg operand')
generateInstructions iReg _ (Unary state Neg operand) = do
    operand' <- lift $ findReg RInt operand
    addInst $ IIntOp (getLoc state) PSub iReg (zeroReg RInt) (Reg operand')
generateInstructions _ fReg (Unary state FNeg operand) = do
    operand' <- lift $ findReg RFloat operand
    addInst $ IFOp (getLoc state) FSub fReg (zeroReg RFloat) operand'
generateInstructions iReg _ (Binary state (RelationOp op) operand1 operand2) = do
    ty <- lift $ liftB $ getTyOf operand1
    withRegType
        ty
        $ \rTy -> do
            operand1' <- lift $ findReg rTy operand1
            operand2' <- lift $ findReg rTy operand2
            addInst $ ICompOp (getLoc state) op iReg operand1' (Reg operand2')
generateInstructions iReg _ (Binary state (IntOp op) operand1 operand2) = do
    const2 <- lift $ liftB $ asConstant operand2
    case (const2, op) of
        (Just (LInt i), Sub) -> do
            operand1' <- lift $ findReg RInt operand1
            addInst $ IIntOp (getLoc state) PAdd iReg operand1' (Imm RInt $ -i)
        (Just (LInt i), _) -> do
            operand1' <- lift $ findReg RInt operand1
            addInst $ IIntOp (getLoc state) (fromIntBinOp op) iReg operand1' (Imm RInt i)
        _ -> do
            operand1' <- lift $ findReg RInt operand1
            operand2' <- lift $ findReg RInt operand2
            addInst $ IIntOp (getLoc state) (fromIntBinOp op) iReg operand1' (Reg operand2')
generateInstructions _ fReg (Binary state (FloatOp op) operand1 operand2) = do
    operand1' <- lift $ findReg RFloat operand1
    operand2' <- lift $ findReg RFloat operand2
    addInst $ IFOp (getLoc state) op fReg operand1' operand2'
generateInstructions iReg fReg (If state cond thenExpr elseExpr) = do
    name <- gets contextName
    labelID <- genNewLabelId
    let thenLabel = name <> "_then_" <> labelID
    let elseLabel = name <> "_else_" <> labelID
    let endIfLabel = name <> "_endif_" <> labelID
    label <- gets currentLabel

    lhsTy <-
        case cond of
            CComp _ lhs _ -> lift $ liftB $ getTyOf lhs
            _ -> pure TInt
    withRegType lhsTy $
        \rTy -> do
            (op, lhs', rhs') <- case cond of
                CComp op lhs rhs -> do
                    lhs' <- lift $ findReg rTy lhs
                    rhs' <- lift $ findReg rTy rhs
                    pure (op, lhs', rhs')
                CIdentity cond' -> do
                    cond'' <- lift $ findReg rTy cond'
                    pure (Ne, cond'', zeroReg rTy)

            terminate $ TBranch op lhs' rhs' thenLabel elseLabel

            iElseReg <- lift $ genTempReg RInt
            fElseReg <- lift $ genTempReg RFloat

            genNewBlock elseLabel [label]
            generateInstructions iElseReg fElseReg elseExpr
            terminate $ TJmp endIfLabel

            iThenReg <- lift $ genTempReg RInt
            fThenReg <- lift $ genTempReg RFloat

            genNewBlock elseLabel [label]
            generateInstructions iThenReg fThenReg thenExpr
            terminate $ TJmp endIfLabel

            genNewBlock endIfLabel [thenLabel, elseLabel]
            addInst $ IPhi (getLoc state) iReg [(thenLabel, iThenReg), (elseLabel, iElseReg)]
            addInst $ IPhi (getLoc state) fReg [(thenLabel, fThenReg), (elseLabel, fElseReg)]
generateInstructions _ _ (Let _ (PRec _ _) _ _) =
    throwError $ OtherError "The function should be removed during closure conversion."
generateInstructions iReg fReg (Let _ PUnit expr body) = do
    generateInstructions iReg fReg expr
    generateInstructions iReg fReg body
generateInstructions iReg fReg (Let _ (PVar v) (Const _ (LInt 0)) body) = do
    lift $ registerReg v (zeroReg RInt)
    generateInstructions iReg fReg body
generateInstructions iReg fReg (Let _ (PVar v) (Const _ (LFloat 0)) body) = do
    lift $ registerReg v (zeroReg RFloat)
    generateInstructions iReg fReg body
generateInstructions iReg fReg (Let _ (PVar v) expr body) = do
    vTy <- lift $ liftB $ getTyOf v
    case vTy of
        TUnit -> do
            let v' = Register RInt DCReg
            lift $ registerReg v v'
            generateInstructions v' fReg expr
            generateInstructions iReg fReg body
        TFloat -> do
            v' <- lift $ genReg RFloat v
            generateInstructions iReg v' expr
            generateInstructions iReg fReg body
        _ -> do
            v' <- lift $ genReg RInt v
            generateInstructions v' fReg expr
            generateInstructions iReg fReg body
generateInstructions iReg fReg (Let state (PTuple vals) expr body) = do
    tuple <- lift $ genTempReg RInt
    generateInstructions tuple fReg expr
    store <-
        mapM
            ( \(idx, val) -> do
                ty <- lift $ liftB $ getTyOf val
                withRegType ty $
                    \rTy -> do
                        val' <- lift $ genReg rTy val
                        pure $ ILoad (getLoc state) val' tuple idx
            )
            $ zip [0 ..] vals
    appendInst store
    generateInstructions iReg fReg body
generateInstructions iReg _ (Var state (ExternalIdent ext)) = do
    -- Possibly a global variable.
    prop <- lift $ findGlobal ext
    let offset = globalOffset prop
    addInst $ IMov (getLoc state) iReg (Imm RInt offset)
generateInstructions iReg fReg (Var state ident) = do
    ty <- lift $ liftB $ getTyOf ident
    case ty of
        TUnit -> do
            pure ()
        TFloat -> do
            regID <- lift $ findReg RFloat ident
            when (fReg /= regID) $
                addInst $
                    IMov (getLoc state) fReg (Reg regID)
        _ -> do
            regID <- lift $ findReg RInt ident
            when (iReg /= regID) $
                addInst $
                    IMov (getLoc state) iReg (Reg regID)
generateInstructions iReg _ (Tuple state vars) = do
    inst <-
        concat
            <$> mapM
                ( \(index, var) -> do
                    case var of
                        ExternalIdent ext -> do
                            -- An element of the tuple can be a global variable.
                            temp <- lift $ genTempReg RInt
                            prop <- lift $ findGlobal ext
                            let offset = globalOffset prop
                            pure
                                [ IMov (getLoc state) temp (Imm RInt offset)
                                , IStore (getLoc state) temp heapReg index
                                ]
                        _ -> do
                            varTy <- lift $ liftB $ getTyOf var
                            withRegType varTy $
                                \rTy -> do
                                    var' <- lift $ findReg rTy var
                                    pure [IStore (getLoc state) var' heapReg index]
                )
                (zip [0 ..] vars)
    appendInst inst
    addInst $ IMov (getLoc state) iReg (Reg heapReg)
    addInst $ IIntOp (getLoc state) PAdd heapReg heapReg (Imm RInt $ length vars)
generateInstructions iReg _ (ArrayCreate state size _) = do
    constSize <- lift $ liftB $ asConstant size
    case constSize of
        (Just (LInt s)) -> do
            addInst $ IMov (getLoc state) iReg (Reg heapReg)
            addInst $ IIntOp (getLoc state) PAdd heapReg heapReg (Imm RInt s)
        _ -> do
            size' <- lift $ findReg RInt size
            addInst $ IMov (getLoc state) iReg (Reg heapReg)
            addInst $ IIntOp (getLoc state) PAdd heapReg heapReg (Reg size')
generateInstructions iReg fReg (Get state array index) = do
    array' <- lift $ findRegOrImm RInt array
    constIndex <- lift $ liftB $ asConstant index
    case (array', constIndex) of
        (Imm _ offset, Just (LInt index')) -> do
            -- If both of them are constants, we can calculate the address at compile time.
            let ty = getType state
            case ty of
                TFloat ->
                    addInst $ ILoad (getLoc state) fReg (zeroReg RInt) (offset + index')
                _ ->
                    addInst $ ILoad (getLoc state) iReg (zeroReg RInt) (offset + index')
        (Imm _ offset, Nothing) -> do
            -- If the array is a constant and the index is a variable, we can calculate the address at compile time.
            index' <- lift $ findReg RInt index
            let ty = getType state
            case ty of
                TFloat ->
                    addInst $ ILoad (getLoc state) fReg index' offset
                _ ->
                    addInst $ ILoad (getLoc state) iReg index' offset
        (Reg reg, Just (LInt index')) -> do
            -- If the array is a register and the index is a constant, we can calculate the address at compile time.
            let ty = getType state
            case ty of
                TFloat ->
                    addInst $ ILoad (getLoc state) fReg reg index'
                _ ->
                    addInst $ ILoad (getLoc state) iReg reg index'
        _ -> do
            -- If both of them are variables, we need to calculate the address at runtime.
            index' <- lift $ findReg RInt index
            addr <- lift $ genTempReg RInt
            let ty = getType state
            case ty of
                TFloat -> do
                    addInst $ IIntOp (getLoc state) PAdd addr index' array'
                    addInst $ ILoad (getLoc state) fReg addr 0
                _ -> do
                    addInst $ IIntOp (getLoc state) PAdd addr index' array'
                    addInst $ ILoad (getLoc state) iReg addr 0
generateInstructions _ _ (Put state dest idx src) = do
    dest' <- lift $ findRegOrImm RInt dest
    constIdx <- lift $ liftB $ asConstant idx

    srcTy <- lift $ liftB $ getTyOf src
    case srcTy of
        TFloat -> do
            src' <- lift $ findReg RFloat src
            case (dest', constIdx) of
                (Imm _ offset, Just (LInt index')) -> do
                    -- If both of them are constants, we can calculate the address at compile time.
                    addInst $ IStore (getLoc state) src' (zeroReg RInt) (offset + index')
                (Imm _ offset, Nothing) -> do
                    -- If the array is a constant and the index is a variable, we can calculate the address at compile time.
                    index' <- lift $ findReg RInt idx
                    addInst $ IStore (getLoc state) src' index' offset
                (Reg reg, Just (LInt index')) -> do
                    -- If the array is a register and the index is a constant, we can calculate the address at compile time.
                    addInst $ IStore (getLoc state) src' reg index'
                (Reg reg, Nothing) -> do
                    -- If both of them are variables, we need to calculate the address at runtime.
                    index' <- lift $ findReg RInt idx
                    addr <- lift $ genTempReg RInt
                    addInst $ IIntOp (getLoc state) PAdd addr reg (Reg index')
                    addInst $ IStore (getLoc state) src' addr 0
                _ -> throwError $ OtherError "The index should be integer."
        _ -> do
            -- Create a new register if the source is external.
            src' <- lift $ findRegOrImm RInt src
            srcReg <- case src' of
                Reg reg -> pure reg
                Imm _ imm -> do
                    srcReg <- lift $ genTempReg RInt
                    addInst $ IMov (getLoc state) srcReg (Imm RInt imm)
                    pure srcReg

            case (dest', constIdx) of
                (Imm _ offset, Just (LInt index')) -> do
                    -- If both of them are constants, we can calculate the address at compile time.
                    addInst $ IStore (getLoc state) srcReg (zeroReg RInt) (offset + index')
                (Imm _ offset, _) -> do
                    -- If the array is a constant and the index is a variable, we can calculate the address at compile time.
                    index' <- lift $ findReg RInt idx
                    addInst $ IStore (getLoc state) srcReg index' offset
                (Reg reg, Just (LInt index')) -> do
                    -- If the array is a register and the index is a constant, we can calculate the address at compile time.
                    addInst $ IStore (getLoc state) srcReg reg index'
                (Reg reg, _) -> do
                    -- If both of them are variables, we need to calculate the address at runtime.
                    index' <- lift $ findReg RInt idx
                    addr <- lift $ genTempReg RInt
                    addInst $ IIntOp (getLoc state) PAdd addr reg (Reg index')
                    addInst $ IStore (getLoc state) srcReg addr 0
generateInstructions iReg _ (MakeClosure state func freeV) = do
    iFreeV <- lift $ filterVars RInt freeV
    fFreeV <- lift $ filterVars RFloat freeV
    -- Need to proof iFreeV and fFreeV don't contain any global variables.
    iFreeV' <- mapM (lift . findRegOrImm RInt) iFreeV
    fFreeV' <- mapM (lift . findReg RFloat) fFreeV
    addInst $ IMakeClosure (getLoc state) iReg (display func) iFreeV' fFreeV'
generateInstructions iReg fReg (DirectApp state func args) = do
    case args of
        (ExternalIdent tuple) : values -> do
            if func == builtinMakeTuple
                then
                    mkTuple tuple values
                else
                    simpleApp
        _ -> simpleApp
  where
    mkTuple :: (Monad m) => Text -> [Ident] -> CodeBlockStateT m ()
    mkTuple tuple values = do
        prop <- lift $ findGlobal tuple
        let offset = globalOffset prop
        mapM_
            ( \(idx, val) -> do
                ty <- lift $ liftB $ getTyOf val
                withRegType ty $
                    \rTy -> do
                        reg <- lift $ findRegOrImm rTy val
                        case reg of
                            Reg reg' -> do
                                addInst $ IStore (getLoc state) reg' (zeroReg RInt) (offset + idx)
                            Imm RInt imm -> do
                                temp' <- lift $ genTempReg RInt
                                addInst $ IMov (getLoc state) temp' (Imm RInt imm)
                                addInst $ IStore (getLoc state) temp' (zeroReg RInt) (offset + idx)
                            Imm RFloat _ -> do
                                throwError $ OtherError "The address cannot be float."
            )
            ( zip
                [0 ..]
                values
            )

    simpleApp ::
        (Monad m) =>
        CodeBlockStateT m ()
    simpleApp = do
        (iArgs', fArgs') <- lift $ resolveArgs args
        case getType state of
            TUnit -> do
                case findBuiltin func of
                    Just builtin -> do
                        addInst $ IRawInst (getLoc state) (builtinInst builtin) (Register RInt DCReg) iArgs' fArgs'
                    Nothing -> do
                        addInst $ IRichCall (getLoc state) (display func) iArgs' fArgs'
            TFloat -> do
                case findBuiltin func of
                    Just builtin -> do
                        addInst $ IRawInst (getLoc state) (builtinInst builtin) fReg iArgs' fArgs'
                    Nothing -> do
                        addInst $ IRichCall (getLoc state) (display func) iArgs' fArgs'
                        addInst $ IMov (getLoc state) fReg (Reg $ retReg RFloat)
            _ -> do
                case findBuiltin func of
                    Just builtin -> do
                        addInst $ IRawInst (getLoc state) (builtinInst builtin) iReg iArgs' fArgs'
                    Nothing -> do
                        addInst $ IRichCall (getLoc state) (display func) iArgs' fArgs'
                        addInst $ IMov (getLoc state) iReg (Reg $ retReg RInt)
generateInstructions iReg fReg (ClosureApp state func args) = do
    func' <- lift $ findReg RInt func
    (iArgs', fArgs') <- lift $ resolveArgs args
    case getType state of
        TUnit -> do
            addInst $ IClosureCall (getLoc state) func' iArgs' fArgs'
        TFloat -> do
            addInst $ IClosureCall (getLoc state) func' iArgs' fArgs'
            addInst $ IMov (getLoc state) fReg (Reg $ retReg RFloat)
        _ -> do
            addInst $ IClosureCall (getLoc state) func' iArgs' fArgs'
            addInst $ IMov (getLoc state) iReg (Reg $ retReg RInt)
generateInstructions iReg fReg (Loop state args values body) = do
    prevLoopLabel <- gets loopLabel
    prevILoopArgs <- gets iLoopArgs
    prevFLoopArgs <- gets fLoopArgs

    current <- gets currentLabel

    name <- gets contextName
    labelID <- genNewLabelId
    let loopLabel' = name <> "_loop_" <> labelID

    iArgs <- lift $ filterVars RInt args
    fArgs <- lift $ filterVars RFloat args

    iArgs' <- mapM (lift . genReg RInt) iArgs
    fArgs' <- mapM (lift . genReg RFloat) fArgs

    iValues <- lift $ filterVars RInt values
    fValues <- lift $ filterVars RFloat values

    iValues' <- mapM (takeReg RInt) iValues
    fValues' <- mapM (takeReg RFloat) fValues

    iLoopReg <- mapM (\_ -> lift $ genTempReg RInt) args
    fLoopReg <- mapM (\_ -> lift $ genTempReg RFloat) args
    modify $ \ctx ->
        ctx
            { loopLabel = loopLabel'
            , iLoopArgs = iLoopReg
            , fLoopArgs = fLoopReg
            }

    terminate $ TJmp loopLabel'

    genNewBlock loopLabel' [current]

    mapM_ (\(a, b, c) -> addInst $ IPhi (getLoc state) a [(current, b), ("unknown", c)]) $ zip3 iArgs' iValues' iLoopReg
    mapM_ (\(a, b, c) -> addInst $ IPhi (getLoc state) a [(current, b), ("unknown", c)]) $ zip3 fArgs' fValues' fLoopReg
    generateInstructions iReg fReg body

    modify $ \ctx ->
        ctx
            { loopLabel = prevLoopLabel
            , iLoopArgs = prevILoopArgs
            , fLoopArgs = prevFLoopArgs
            }
generateInstructions _ _ (Continue state args) = do
    iLoopReg <- gets iLoopArgs
    fLoopReg <- gets fLoopArgs
    label <- gets loopLabel

    iArgs <- lift $ filterVars RInt args
    fArgs <- lift $ filterVars RFloat args

    iArgs' <- mapM (lift . findRegOrImm RInt) iArgs
    fArgs' <- mapM (lift . findRegOrImm RFloat) fArgs

    mapM_ (\(a, b) -> addInst $ IMov (getLoc state) a b) $ zip iLoopReg iArgs'
    mapM_ (\(a, b) -> addInst $ IMov (getLoc state) a b) $ zip fLoopReg fArgs'

    terminate $ TJmp label

    genNewBlock "unreachable" []

generateCodeBlock ::
    (Monad m) =>
    Function ->
    CodeBlockStateT m ()
generateCodeBlock (Function _ _ _ freeVars boundedVars body) = do
    boundedVarsInstructions RInt
    boundedVarsInstructions RFloat

    iArgsLength <- lift $ gets (argsLength . (#!! RInt) . regContext)
    freeVarsInstructions iArgsLength

    generateInstructions (retReg RInt) (retReg RFloat) body

    terminate TReturn
  where
    freeVarsInstructions :: (Monad m) => Int -> CodeBlockStateT m ()
    freeVarsInstructions closureArg = do
        iFreeRegisters <- lift $ genRegisters RInt freeVars
        fFreeRegisters <- lift $ genRegisters RFloat freeVars
        mapM_
            ( \(num, reg) ->
                addInst $ ILoad dummyLoc reg argReg num
            )
            $ zip [0 ..] iFreeRegisters
        mapM_
            ( \(num, reg) ->
                addInst $ ILoad dummyLoc reg argReg num
            )
            $ zip [length iFreeRegisters ..] fFreeRegisters
      where
        argReg = argsReg RInt closureArg

    boundedVarsInstructions :: (Monad m) => RegType rTy -> CodeBlockStateT m ()
    boundedVarsInstructions rTy = do
        boundedRegisters <- lift $ genRegisters rTy boundedVars
        lift $ updateRegContext rTy $ \ctx -> ctx{argsLength = length boundedRegisters}
        mapM_
            ( \(num, reg) ->
                addInst $ IMov dummyLoc reg (Reg (argsReg rTy num))
            )
            $ zip [0 ..] boundedRegisters

generateBlockGraph ::
    (Monad m) =>
    Function ->
    BackendIdentState m (BlockGraph AbstInstKind)
generateBlockGraph func = do
    ctx <-
        execStateT (generateCodeBlock func) $
            CodeBlockContext
                { currentLabel = funcLabel
                , instBuf = mempty
                , generatedBlocks = []
                , contextName = funcLabel
                , generatedID = 0
                , prevLabels = []
                , loopLabel = "unreachable"
                , iLoopArgs = []
                , fLoopArgs = []
                }
    pure $ BlockGraph (generatedBlocks ctx) funcLabel
  where
    funcLabel = display $ funcName func

expandExprToInst ::
    (Monad m) =>
    Register RegID Int ->
    Register RegID Float ->
    ClosureExpr ->
    BackendIdentState m [AbstInst]
expandExprToInst _ _ (Const _ LUnit) = do
    pure []
expandExprToInst iReg _ (Const state (LInt i)) = do
    pure [IMov (getLoc state) iReg (Imm RInt i)]
expandExprToInst iReg _ (Const state (LBool b)) = do
    pure [IMov (getLoc state) iReg (Imm RInt $ if b then 1 else 0)]
expandExprToInst _ fReg (Const state (LFloat f)) = do
    pure [IMov (getLoc state) fReg (Imm RFloat f)]
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
    withRegType
        ty
        $ \rTy -> do
            operand1' <- findReg rTy operand1
            operand2' <- findReg rTy operand2
            pure [ICompOp (getLoc state) op iReg operand1' (Reg operand2')]
expandExprToInst iReg _ (Binary state (IntOp op) operand1 operand2) = do
    const2 <- liftB $ asConstant operand2
    case (const2, op) of
        (Just (LInt i), Sub) -> do
            operand1' <- findReg RInt operand1
            pure [IIntOp (getLoc state) PAdd iReg operand1' (Imm RInt $ -i)]
        (Just (LInt i), _) -> do
            operand1' <- findReg RInt operand1
            pure [IIntOp (getLoc state) (fromIntBinOp op) iReg operand1' (Imm RInt i)]
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
    withRegType lhsTy $
        \rTy -> do
            lhs' <- findReg rTy lhs
            rhs' <- findReg rTy rhs

            pure [IBranch (getLoc state) op lhs' rhs' thenExpr' elseExpr']
expandExprToInst _ _ (Let _ (PRec _ _) _ _) =
    throwError $ OtherError "The function should be removed during closure conversion."
expandExprToInst iReg fReg (Let _ PUnit expr body) = do
    expr' <- expandExprToInst iReg fReg expr
    body' <- expandExprToInst iReg fReg body
    pure $ expr' ++ body'
expandExprToInst iReg fReg (Let _ (PVar v) (Const _ (LInt 0)) body) = do
    registerReg v (zeroReg RInt)
    expandExprToInst iReg fReg body
expandExprToInst iReg fReg (Let _ (PVar v) (Const _ (LFloat 0)) body) = do
    registerReg v (zeroReg RFloat)
    expandExprToInst iReg fReg body
expandExprToInst iReg fReg (Let _ (PVar v) expr body) = do
    vTy <- liftB $ getTyOf v
    case vTy of
        TUnit -> do
            let v' = Register RInt DCReg
            registerReg v v'
            expr' <- expandExprToInst v' fReg expr
            body' <- expandExprToInst iReg fReg body
            pure $ expr' ++ body'
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
                withRegType ty $
                    \rTy -> do
                        val' <- genReg rTy val
                        pure $ ILoad (getLoc state) val' tuple idx
            )
            $ zip [0 ..] vals
    body' <- expandExprToInst iReg fReg body
    pure $ expr' ++ store ++ body'
expandExprToInst iReg _ (Var state (ExternalIdent ext)) = do
    -- Possibly a global variable.
    prop <- findGlobal ext
    let offset = globalOffset prop
    pure [IMov (getLoc state) iReg (Imm RInt offset)]
expandExprToInst iReg fReg (Var state ident) = do
    ty <- liftB $ getTyOf ident
    case ty of
        TUnit -> do
            pure []
        TFloat -> do
            regID <- findReg RFloat ident
            if fReg /= regID
                then pure [IMov (getLoc state) fReg (Reg regID)]
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
                                [ IMov (getLoc state) temp (Imm RInt offset)
                                , IStore (getLoc state) temp heapReg index
                                ]
                        _ -> do
                            varTy <- liftB $ getTyOf var
                            withRegType varTy $
                                \rTy -> do
                                    var' <- findReg rTy var
                                    pure [IStore (getLoc state) var' heapReg index]
                )
                (zip [0 ..] vars)
    pure $
        inst
            ++ [ IMov (getLoc state) iReg (Reg heapReg)
               , IIntOp (getLoc state) PAdd heapReg heapReg (Imm RInt $ length vars)
               ]
expandExprToInst iReg _ (ArrayCreate state size _) = do
    size' <- findReg RInt size
    pure
        [ IMov (getLoc state) iReg (Reg heapReg)
        , IIntOp (getLoc state) PAdd heapReg heapReg (Reg size')
        ]
expandExprToInst iReg fReg (Get state array index) = do
    array' <- findRegOrImm RInt array
    constIndex <- liftB $ asConstant index
    case (array', constIndex) of
        (Imm _ offset, Just (LInt index')) -> do
            -- If both of them are constants, we can calculate the address at compile time.
            let ty = getType state
            case ty of
                TFloat ->
                    pure
                        [ ILoad (getLoc state) fReg (zeroReg RInt) (offset + index')
                        ]
                _ ->
                    pure
                        [ ILoad (getLoc state) iReg (zeroReg RInt) (offset + index')
                        ]
        (Imm _ offset, Nothing) -> do
            -- If the array is a constant and the index is a variable, we can calculate the address at compile time.
            index' <- findReg RInt index
            let ty = getType state
            case ty of
                TFloat ->
                    pure
                        [ ILoad (getLoc state) fReg index' offset
                        ]
                _ ->
                    pure
                        [ ILoad (getLoc state) iReg index' offset
                        ]
        (Reg reg, Just (LInt index')) -> do
            -- If the array is a register and the index is a constant, we can calculate the address at compile time.
            let ty = getType state
            case ty of
                TFloat ->
                    pure
                        [ ILoad (getLoc state) fReg reg index'
                        ]
                _ ->
                    pure
                        [ ILoad (getLoc state) iReg reg index'
                        ]
        _ -> do
            -- If both of them are variables, we need to calculate the address at runtime.
            index' <- findReg RInt index
            addr <- genTempReg RInt
            let ty = getType state
            case ty of
                TFloat ->
                    pure
                        [ IIntOp (getLoc state) PAdd addr index' array'
                        , ILoad (getLoc state) fReg addr 0
                        ]
                _ ->
                    pure
                        [ IIntOp (getLoc state) PAdd addr index' array'
                        , ILoad (getLoc state) iReg addr 0
                        ]
expandExprToInst _ _ (Put state dest idx src) = do
    dest' <- findRegOrImm RInt dest
    constIdx <- liftB $ asConstant idx

    srcTy <- liftB $ getTyOf src
    case srcTy of
        TFloat -> do
            src' <- findReg RFloat src
            case (dest', constIdx) of
                (Imm _ offset, Just (LInt index')) -> do
                    -- If both of them are constants, we can calculate the address at compile time.
                    pure
                        [ IStore (getLoc state) src' (zeroReg RInt) (offset + index')
                        ]
                (Imm _ offset, Nothing) -> do
                    -- If the array is a constant and the index is a variable, we can calculate the address at compile time.
                    index' <- findReg RInt idx
                    pure
                        [ IStore (getLoc state) src' index' offset
                        ]
                (Reg reg, Just (LInt index')) -> do
                    -- If the array is a register and the index is a constant, we can calculate the address at compile time.
                    pure
                        [ IStore (getLoc state) src' reg index'
                        ]
                (Reg reg, Nothing) -> do
                    -- If both of them are variables, we need to calculate the address at runtime.
                    index' <- findReg RInt idx
                    addr <- genTempReg RInt
                    pure
                        [ IIntOp (getLoc state) PAdd addr reg (Reg index')
                        , IStore (getLoc state) src' addr 0
                        ]
                _ -> throwError $ OtherError "The index should be integer."
        _ -> do
            -- Create a new register if the source is external.
            src' <- findRegOrImm RInt src
            (prologue, srcReg) <- case src' of
                Reg reg -> pure ([], reg)
                Imm _ imm -> do
                    srcReg <- genTempReg RInt
                    pure ([IMov (getLoc state) srcReg (Imm RInt imm)], srcReg)

            case (dest', constIdx) of
                (Imm _ offset, Just (LInt index')) -> do
                    -- If both of them are constants, we can calculate the address at compile time.
                    pure $
                        prologue
                            ++ [ IStore (getLoc state) srcReg (zeroReg RInt) (offset + index')
                               ]
                (Imm _ offset, _) -> do
                    -- If the array is a constant and the index is a variable, we can calculate the address at compile time.
                    index' <- findReg RInt idx
                    pure $
                        prologue
                            ++ [ IStore (getLoc state) srcReg index' offset
                               ]
                (Reg reg, Just (LInt index')) -> do
                    -- If the array is a register and the index is a constant, we can calculate the address at compile time.
                    pure $
                        prologue
                            ++ [ IStore (getLoc state) srcReg reg index'
                               ]
                (Reg reg, _) -> do
                    -- If both of them are variables, we need to calculate the address at runtime.
                    index' <- findReg RInt idx
                    addr <- genTempReg RInt
                    pure $
                        prologue
                            ++ [ IIntOp (getLoc state) PAdd addr reg (Reg index')
                               , IStore (getLoc state) srcReg addr 0
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
    mkTuple :: (Monad m) => Text -> [Ident] -> BackendIdentState m [AbstInst]
    mkTuple tuple values = do
        prop <- findGlobal tuple
        let offset = globalOffset prop
        concat
            <$> mapM
                ( \(idx, val) -> do
                    ty <- liftB $ getTyOf val
                    withRegType ty $
                        \rTy -> do
                            reg <- findRegOrImm rTy val
                            case reg of
                                Reg reg' -> do
                                    pure [IStore (getLoc state) reg' (zeroReg RInt) (offset + idx)]
                                Imm RInt imm -> do
                                    temp' <- genTempReg RInt
                                    pure
                                        [ IMov (getLoc state) temp' (Imm RInt imm)
                                        , IStore (getLoc state) temp' (zeroReg RInt) (offset + idx)
                                        ]
                                Imm RFloat _ -> do
                                    throwError $ OtherError "The address cannot be float."
                )
                ( zip
                    [0 ..]
                    values
                )

    simpleApp ::
        (Monad m) =>
        BackendIdentState m [AbstInst]
    simpleApp = do
        (iArgs', fArgs') <- resolveArgs args
        case getType state of
            TUnit -> do
                case findBuiltin func of
                    Just builtin -> do
                        pure [IRawInst (getLoc state) (builtinInst builtin) (Register RInt DCReg) iArgs' fArgs']
                    Nothing -> do
                        pure [IRichCall (getLoc state) (display func) iArgs' fArgs']
            TFloat -> do
                case findBuiltin func of
                    Just builtin -> do
                        pure [IRawInst (getLoc state) (builtinInst builtin) fReg iArgs' fArgs']
                    Nothing -> do
                        pure
                            [ IRichCall (getLoc state) (display func) iArgs' fArgs'
                            , IMov (getLoc state) fReg (Reg $ retReg RFloat)
                            ]
            _ -> do
                case findBuiltin func of
                    Just builtin -> do
                        pure [IRawInst (getLoc state) (builtinInst builtin) iReg iArgs' fArgs']
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
                , IMov (getLoc state) fReg (Reg $ retReg RFloat)
                ]
        _ -> do
            pure
                [ IClosureCall (getLoc state) func' iArgs' fArgs'
                , IMov (getLoc state) iReg (Reg $ retReg RInt)
                ]

extractFreeVars :: (Monad m) => Int -> [Ident] -> BackendIdentState m [AbstInst]
extractFreeVars closureArg freeVars = do
    iFreeRegisters <- genRegisters RInt freeVars
    fFreeRegisters <- genRegisters RFloat freeVars
    iLoadInst <-
        mapM
            ( \(num, reg) ->
                pure $ ILoad dummyLoc reg argReg num
            )
            $ zip [0 ..] iFreeRegisters
    fLoadInst <-
        mapM
            ( \(num, reg) ->
                pure $ ILoad dummyLoc reg argReg num
            )
            $ zip [length iFreeRegisters ..] fFreeRegisters

    pure $ iLoadInst ++ fLoadInst
  where
    argReg = argsReg RInt closureArg

extractBoundedVars :: (Monad m) => RegType rTy -> [Ident] -> BackendIdentState m [AbstInst]
extractBoundedVars RInt boundedVars = do
    boundedRegisters <- genRegisters RInt boundedVars
    updateRegContext RInt $ \ctx -> ctx{argsLength = length boundedRegisters}
    mapM
        ( \(num, reg) ->
            pure $ IMov dummyLoc reg (Reg (argsReg RInt num))
        )
        $ zip [0 ..] boundedRegisters
extractBoundedVars RFloat boundedVars = do
    boundedRegisters <- genRegisters RFloat boundedVars
    updateRegContext RFloat $ \ctx -> ctx{argsLength = length boundedRegisters}
    mapM
        ( \(num, reg) ->
            pure $ IMov dummyLoc reg (Reg (argsReg RFloat num))
        )
        $ zip [0 ..] boundedRegisters

toInstructions :: (Monad m) => Function -> BackendIdentState m AbstCodeBlock
toInstructions function = do
    inst <- toInstructions' function
    pure $ HCodeBlock (display $ funcName function) 0 inst
  where
    toInstructions' :: (Monad m) => Function -> BackendIdentState m [AbstInst]
    toInstructions' (Function _ _ _ freeVars boundedArgs body) = do
        iBoundedInst <- extractBoundedVars RInt boundedArgs
        fBoundedInst <- extractBoundedVars RFloat boundedArgs

        iArgsLength <- gets (argsLength . (#!! RInt) . regContext)
        freeInst <- extractFreeVars iArgsLength freeVars

        inst <- expandExprToInst (retReg RInt) (retReg RFloat) body
        pure $ iBoundedInst ++ fBoundedInst ++ freeInst ++ inst
