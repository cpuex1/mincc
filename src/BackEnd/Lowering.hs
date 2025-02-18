{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module BackEnd.Lowering (
    RegID,
    BackendIdentState,
    generateBlockGraph,
) where

import BackEnd.Analysis.CodeBlock (fillInPrevBlocks)
import BackEnd.BackendEnv
import BackEnd.Optim.Common (insertPhi)
import Builtin (BuiltinFunction (builtinInst), builtinMakeTuple, findBuiltin)
import CodeBlock (
    BlockGraph (BlockGraph),
    CodeBlock (CodeBlock, blockInst, blockName),
    Terminator (TBranch, TJmp, TReturn),
 )
import Control.Monad (filterM, when)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (MonadTrans (lift), StateT, execStateT, gets, modify)
import Data.Foldable (Foldable (toList))
import Data.Map (insert)
import qualified Data.Map as M
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
    dcReg,
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
    getExprState,
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
genRegisters :: (Monad m) => RegType rTy -> [Ident] -> BackendIdentState m [Register rTy]
genRegisters rTy vars = do
    filtered <- filterVars rTy vars
    mapM (genReg rTy) filtered

resolveArgs :: (Monad m) => [Ident] -> BackendIdentState m ([RegOrImm Int], [Register Float])
resolveArgs args = do
    iArgs <- filterVars RInt args
    fArgs <- filterVars RFloat args
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
    , iLoopArgs :: [Register Int]
    , fLoopArgs :: [Register Float]
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

terminate :: (Monad m) => Terminator -> CodeBlockStateT m ()
terminate term = do
    modify $ \ctx ->
        ctx
            { generatedBlocks =
                generatedBlocks ctx
                    ++ [CodeBlock (currentLabel ctx) (toList $ instBuf ctx) (prevLabels ctx) term]
            }

-- | Add a phi instruction to the loop.
addLoopPhi :: (Monad m) => Register a -> Register a -> CodeBlockStateT m ()
addLoopPhi target src = do
    loop <- gets loopLabel
    current <- gets currentLabel
    if current == loop
        then do
            -- A loop is generating.
            -- Add a phi instruction to the current block.
            buffer <- gets (toList . instBuf)
            let newBuffer = insertPhi dummyLoc target current src buffer
            modify $ \ctx -> ctx{instBuf = fromList newBuffer}
        else do
            -- A loop is already generated.
            generated <- gets generatedBlocks
            let generated' =
                    map
                        ( \block ->
                            if blockName block == loop
                                then
                                    let newInst = insertPhi dummyLoc target current src (blockInst block)
                                     in block{blockInst = newInst}
                                else block
                        )
                        generated
            modify $ \ctx -> ctx{generatedBlocks = generated'}

genNewLabelId :: (Monad m) => CodeBlockStateT m Text
genNewLabelId = do
    labelID <- gets generatedID
    modify $ \ctx' -> ctx'{generatedID = labelID + 1}
    pure $ display labelID

resolveRegOrImm :: (Monad m) => RegOrImm rTy -> CodeBlockStateT m (Register rTy)
resolveRegOrImm (Reg reg) = pure reg
resolveRegOrImm (Imm ty imm) = do
    temp <- lift $ genTempReg ty
    addInst $ IMov dummyLoc temp (Imm ty imm)
    pure temp

takeReg :: (Monad m) => RegType rTy -> Ident -> CodeBlockStateT m (Register rTy)
takeReg rTy ident = lift (findRegOrImm rTy ident) >>= resolveRegOrImm

generateInstructions ::
    (Monad m) =>
    Register Int ->
    Register Float ->
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

            -- There are no guarantee that the label has not been changed.
            currentElseLabel <- gets currentLabel
            terminate $ TJmp endIfLabel

            iThenReg <- lift $ genTempReg RInt
            fThenReg <- lift $ genTempReg RFloat

            genNewBlock thenLabel [label]
            generateInstructions iThenReg fThenReg thenExpr

            -- There are no guarantee that the label has not been changed.
            currentThenLabel <- gets currentLabel
            terminate $ TJmp endIfLabel

            genNewBlock endIfLabel [currentThenLabel, currentElseLabel]
            withRegType branchType $
                \case
                    RInt ->
                        addInst $ IPhi (getLoc state) iReg (insert currentThenLabel iThenReg $ M.singleton currentElseLabel iElseReg)
                    RFloat ->
                        addInst $ IPhi (getLoc state) fReg (insert currentThenLabel fThenReg $ M.singleton currentElseLabel fElseReg)
  where
    branchType = getType state
generateInstructions _ _ (Let _ (PRec _ _) _ _) =
    throwError $ OtherError "The function should be removed during closure conversion."
generateInstructions iReg fReg (Let _ PUnit expr body) = do
    generateInstructions iReg fReg expr
    generateInstructions iReg fReg body
generateInstructions iReg fReg (Let state (PVar v) (Const _ (LInt 0)) body) = do
    reg <- lift $ genReg RInt v
    addInst $ IMov (getLoc state) reg (Reg $ zeroReg RInt)
    generateInstructions iReg fReg body
generateInstructions iReg fReg (Let state (PVar v) (Const _ (LFloat 0)) body) = do
    reg <- lift $ genReg RFloat v
    addInst $ IMov (getLoc state) reg (Reg $ zeroReg RFloat)
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
    -- Store the function label.
    labelReg <- lift $ genTempReg RInt
    addInst $ ILMov (getLoc state) labelReg (display func)
    addInst $ IStore (getLoc state) labelReg heapReg 0

    -- Store the free variables.
    (iFreeV, fFreeV) <- lift $ resolveArgs freeV
    mapM_
        ( \(v, offset) -> do
            reg <- resolveRegOrImm v
            addInst $ IStore (getLoc state) reg heapReg offset
        )
        $ zip iFreeV [1 ..]
    mapM_
        ( \(v, offset) -> do
            addInst $ IStore (getLoc state) v heapReg offset
        )
        $ zip fFreeV [(1 + length iFreeV) ..]

    -- Get the address of the closure.
    addInst $ IMov (getLoc state) iReg (Reg heapReg)
    addInst $ IIntOp (getLoc state) PAdd heapReg heapReg (Imm RInt $ 1 + length freeV)
generateInstructions iReg fReg (DirectApp state func args) = do
    if func == builtinMakeTuple
        then do
            -- The builtin function for initializing global tuples.
            case args of
                (ExternalIdent tuple) : values -> do
                    prop <- lift $ findGlobal tuple
                    let offset = globalOffset prop
                    mapM_
                        ( \(idx, val) -> do
                            ty <- lift $ liftB $ getTyOf val
                            withRegType ty $
                                \rTy -> do
                                    reg <- lift $ findRegOrImm rTy val
                                    reg' <- resolveRegOrImm reg
                                    addInst $ IStore (getLoc state) reg' (zeroReg RInt) (offset + idx)
                        )
                        ( zip
                            [0 ..]
                            values
                        )
                _ -> throwError $ OtherError "The first argument should be a tuple."
        else do
            -- A simple function application or a special instruction.
            (iArgs', fArgs') <- lift $ resolveArgs args

            case findBuiltin func of
                Just builtin -> do
                    -- A special instruction.
                    case getType state of
                        TUnit -> do
                            addInst $ IRawInst (getLoc state) (builtinInst builtin) (Register RInt DCReg) iArgs' fArgs'
                        TFloat -> do
                            addInst $ IRawInst (getLoc state) (builtinInst builtin) fReg iArgs' fArgs'
                        _ -> do
                            addInst $ IRawInst (getLoc state) (builtinInst builtin) iReg iArgs' fArgs'
                Nothing -> do
                    -- A simple function application.

                    -- Assign bounded function arguments.
                    mapM_
                        ( \(v, num) -> do
                            addInst $ IMov (getLoc state) (argsReg RInt num) v
                        )
                        $ zip iArgs' [0 ..]
                    mapM_
                        ( \(v, num) -> do
                            addInst $ IMov (getLoc state) (argsReg RFloat num) (Reg v)
                        )
                        $ zip fArgs' [0 ..]

                    -- Call the function.
                    addInst $ ICall (getLoc state) (display func)

                    -- Assign the return value.
                    case getType state of
                        TUnit -> do
                            pure ()
                        TFloat -> do
                            addInst $ IMov (getLoc state) fReg (Reg $ retReg RFloat)
                        _ -> do
                            addInst $ IMov (getLoc state) iReg (Reg $ retReg RInt)
generateInstructions iReg fReg (ClosureApp state closure args) = do
    closure' <- lift $ findReg RInt closure
    func <- lift $ genTempReg RInt
    (iArgs', fArgs') <- lift $ resolveArgs args

    -- Assign bounded function arguments.
    mapM_
        ( \(v, num) -> do
            addInst $ IMov (getLoc state) (argsReg RInt num) v
        )
        $ zip iArgs' [0 ..]
    mapM_
        ( \(v, num) -> do
            addInst $ IMov (getLoc state) (argsReg RFloat num) (Reg v)
        )
        $ zip fArgs' [0 ..]

    -- Call the closure.
    addInst $ IIntOp (getLoc state) PAdd (argsReg RInt $ length iArgs') closure' (Imm RInt 1)
    addInst $ ILoad (getLoc state) func closure' 0
    addInst $ ICallReg (getLoc state) func

    -- Assign the return value.
    case getType state of
        TUnit -> do
            pure ()
        TFloat -> do
            addInst $ IMov (getLoc state) fReg (Reg $ retReg RFloat)
        _ -> do
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

    -- To avoid different registers being merged by phi instructions,
    -- we need to create new registers.
    iValues' <-
        mapM
            ( \val -> do
                dest <- lift $ genTempReg RInt
                src <- lift $ findRegOrImm RInt val
                addInst $ IMov (getLoc state) dest src
                pure dest
            )
            iValues
    fValues' <-
        mapM
            ( \val -> do
                dest <- lift $ genTempReg RFloat
                src <- lift $ findRegOrImm RFloat val
                addInst $ IMov (getLoc state) dest src
                pure dest
            )
            fValues

    modify $ \ctx ->
        ctx
            { loopLabel = loopLabel'
            , iLoopArgs = iArgs'
            , fLoopArgs = fArgs'
            }

    terminate $ TJmp loopLabel'

    genNewBlock loopLabel' [current]

    case loopType of
        TFloat ->
            mapM_ (\(a, b) -> addInst $ IPhi (getLoc state) a $ M.singleton current b) $ zip fArgs' fValues'
        _ ->
            mapM_ (\(a, b) -> addInst $ IPhi (getLoc state) a $ M.singleton current b) $ zip iArgs' iValues'
    generateInstructions iReg fReg body

    modify $ \ctx ->
        ctx
            { loopLabel = prevLoopLabel
            , iLoopArgs = prevILoopArgs
            , fLoopArgs = prevFLoopArgs
            }
  where
    loopType = getType state
generateInstructions _ _ (Continue _ args) = do
    iLoopReg <- gets iLoopArgs
    fLoopReg <- gets fLoopArgs
    label <- gets loopLabel

    iArgs <- lift $ filterVars RInt args
    fArgs <- lift $ filterVars RFloat args

    iArgs' <- mapM (takeReg RInt) iArgs
    fArgs' <- mapM (takeReg RFloat) fArgs

    mapM_ (uncurry addLoopPhi) $ zip iLoopReg iArgs'
    mapM_ (uncurry addLoopPhi) $ zip fLoopReg fArgs'

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

    -- Generate function body.
    -- To prevent return registers from being merged by phi instructions,
    -- we need to create new registers.
    case functionBodyTy of
        TUnit -> do
            generateInstructions (dcReg RInt) (dcReg RFloat) body
        TFloat -> do
            fOut <- lift $ genTempReg RFloat
            generateInstructions (dcReg RInt) fOut body

            addInst $ IMov dummyLoc (retReg RFloat) (Reg fOut)
        _ -> do
            iOut <- lift $ genTempReg RInt
            generateInstructions iOut (dcReg RFloat) body

            addInst $ IMov dummyLoc (retReg RInt) (Reg iOut)

    terminate TReturn
  where
    functionBodyTy = getType $ getExprState body

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
    let graph = BlockGraph (generatedBlocks ctx) funcLabel 0
    pure $ fillInPrevBlocks graph
  where
    funcLabel = display $ funcName func
