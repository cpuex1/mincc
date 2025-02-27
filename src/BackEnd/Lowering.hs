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
import Control.Monad (foldM, when)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (MonadTrans (lift), StateT, execStateT, gets, modify)
import Data.Foldable (Foldable (toList))
import Data.Map (insert)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Sequence (Seq, fromList, (|>))
import Data.Text (Text)
import Display (display)
import Error (CompilerError (OtherError, UnexpectedError))
import IR
import MiddleEnd.Analysis.Identifier (IdentEnvT, asConstant, getTyOf)
import MiddleEnd.Globals (GlobalProp (globalOffset))
import Registers (
    RegID,
    RegList,
    RegOrImm (Imm, Reg),
    RegTuple (createRT),
    RegType (RFloat, RInt),
    RegVariant,
    Register (Register),
    argsReg,
    buildRTM,
    dcReg,
    eachRegType,
    heapReg,
    retReg,
    updateRT,
    withRegType,
    zeroReg,
    (#!!),
 )
import Syntax (
    BinaryOp (FloatOp, IntOp, RelationOp),
    ClosureExpr,
    Cond (CComp, CIdentity, CNeg),
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
import Typing (TypeBase (TInt))

type BackendIdentState m = BackendStateT (IdentEnvT m)

-- | Generate registers for the variables.
genRegAll :: (Monad m) => [Ident] -> BackendIdentState m RegList
genRegAll = do
    foldM
        ( \l var -> do
            ty <- liftB $ getTyOf var
            withRegType ty $
                \case
                    Just rTy -> do
                        reg <- genReg rTy var
                        pure $ updateRT rTy (<> [reg]) l
                    Nothing -> do
                        -- The type is unit.
                        pure $ updateRT RInt (<> [dcReg RInt]) l
        )
        (createRT mempty mempty)

-- | Weakens the register type.
weakenReg :: Register rTy -> RegVariant Register
weakenReg reg@(Register RInt _) = createRT reg (dcReg RFloat)
weakenReg reg@(Register RFloat _) = createRT (dcReg RInt) reg

data CodeBlockContext = CodeBlockContext
    { currentLabel :: InstLabel
    , instBuf :: Seq VirtualInst
    , generatedBlocks :: [CodeBlock VirtualInstKind]
    , contextName :: Text
    , generatedID :: Int
    , prevLabels :: [InstLabel]
    , loopLabel :: InstLabel
    , loopArgs :: RegList
    }

type CodeBlockStateT m = StateT CodeBlockContext (BackendIdentState m)

addInst :: (Monad m) => VirtualInst -> CodeBlockStateT m ()
addInst inst =
    modify $ \ctx -> ctx{instBuf = instBuf ctx |> inst}

appendInst :: (Monad m) => [VirtualInst] -> CodeBlockStateT m ()
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

-- | Search registers at once.
findRegAll :: (Monad m) => [Ident] -> CodeBlockStateT m RegList
findRegAll =
    foldM
        ( \l var -> do
            ty <- lift $ liftB $ getTyOf var
            withRegType ty $
                \case
                    Just rTy -> do
                        reg <- lift (findRegOrImm rTy var)
                        reg' <- resolveRegOrImm reg
                        pure $ updateRT rTy (<> [reg']) l
                    Nothing -> do
                        -- The type is unit.
                        pure $ updateRT RInt (<> [dcReg RInt]) l
        )
        mempty

generateInstructions ::
    (Monad m) =>
    RegVariant Register ->
    ClosureExpr ->
    CodeBlockStateT m ()
generateInstructions _ (Const _ LUnit) =
    pure ()
generateInstructions outReg (Const state (LInt i)) =
    addInst $ IMov (getLoc state) (outReg #!! RInt) (Imm RInt i)
generateInstructions outReg (Const state (LBool b)) =
    addInst $ IMov (getLoc state) (outReg #!! RInt) (Imm RInt $ if b then 1 else 0)
generateInstructions outReg (Const state (LFloat f)) =
    addInst $ IMov (getLoc state) (outReg #!! RFloat) (Imm RFloat f)
generateInstructions outReg (Unary state Not operand) = do
    operand' <- lift $ findReg RInt operand
    addInst $ ICompOp (getLoc state) Eq (outReg #!! RInt) (zeroReg RInt) (Reg operand')
generateInstructions outReg (Unary state Neg operand) = do
    operand' <- lift $ findReg RInt operand
    addInst $ IIntOp (getLoc state) PSub (outReg #!! RInt) (zeroReg RInt) (Reg operand')
generateInstructions outReg (Unary state FNeg operand) = do
    operand' <- lift $ findReg RFloat operand
    addInst $ IFOp (getLoc state) FSub (outReg #!! RFloat) (zeroReg RFloat) operand'
generateInstructions outReg (Binary state (RelationOp op) operand1 operand2) = do
    ty <- lift $ liftB $ getTyOf operand1
    withRegType
        ty
        $ \case
            Just rTy -> do
                operand1' <- lift $ findReg rTy operand1
                operand2' <- lift $ findReg rTy operand2
                addInst $ ICompOp (getLoc state) op (outReg #!! RInt) operand1' (Reg operand2')
            Nothing ->
                throwError $ UnexpectedError "The compare between unit-typed values is not defined."
generateInstructions outReg (Binary state (IntOp op) operand1 operand2) = do
    const2 <- lift $ liftB $ asConstant operand2
    case (const2, op) of
        (Just (LInt i), Sub) -> do
            operand1' <- lift $ findReg RInt operand1
            addInst $ IIntOp (getLoc state) PAdd (outReg #!! RInt) operand1' (Imm RInt $ -i)
        (Just (LInt i), _) -> do
            operand1' <- lift $ findReg RInt operand1
            addInst $ IIntOp (getLoc state) (fromIntBinOp op) (outReg #!! RInt) operand1' (Imm RInt i)
        _ -> do
            operand1' <- lift $ findReg RInt operand1
            operand2' <- lift $ findReg RInt operand2
            addInst $ IIntOp (getLoc state) (fromIntBinOp op) (outReg #!! RInt) operand1' (Reg operand2')
generateInstructions outReg (Binary state (FloatOp op) operand1 operand2) = do
    operand1' <- lift $ findReg RFloat operand1
    operand2' <- lift $ findReg RFloat operand2
    addInst $ IFOp (getLoc state) op (outReg #!! RFloat) operand1' operand2'
generateInstructions outReg (If state cond thenExpr elseExpr) = do
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

    -- Generate the terminator for the current block.
    withRegType lhsTy $
        \case
            Just rTy -> do
                case cond of
                    CComp op lhs rhs -> do
                        lhs' <- lift $ findReg rTy lhs
                        rhs' <- lift $ findReg rTy rhs
                        terminate $ TBranch op lhs' rhs' thenLabel elseLabel
                    CNeg cond' -> do
                        cond'' <- lift $ findReg rTy cond'
                        terminate $ TBranch Eq cond'' (zeroReg rTy) thenLabel elseLabel
                    CIdentity cond' -> do
                        cond'' <- lift $ findReg rTy cond'
                        terminate $ TBranch Ne cond'' (zeroReg rTy) thenLabel elseLabel
            Nothing ->
                throwError $ UnexpectedError "The compare between unit-typed values is not defined."

    withRegType branchType $
        \case
            Just rTy -> do
                elseReg <- lift $ genTempReg rTy

                genNewBlock elseLabel [label]
                generateInstructions (weakenReg elseReg) elseExpr

                -- There are no guarantee that the label has not been changed.
                currentElseLabel <- gets currentLabel
                terminate $ TJmp endIfLabel

                thenReg <- lift $ genTempReg rTy

                genNewBlock thenLabel [label]
                generateInstructions (weakenReg thenReg) thenExpr

                -- There are no guarantee that the label has not been changed.
                currentThenLabel <- gets currentLabel
                terminate $ TJmp endIfLabel

                genNewBlock endIfLabel [currentThenLabel, currentElseLabel]
                addInst $ IPhi (getLoc state) (outReg #!! rTy) (insert currentThenLabel thenReg $ M.singleton currentElseLabel elseReg)
            Nothing -> do
                -- The branch type is unit.
                genNewBlock elseLabel [label]
                generateInstructions (createRT (dcReg RInt) (dcReg RFloat)) elseExpr

                -- There are no guarantee that the label has not been changed.
                currentElseLabel <- gets currentLabel
                terminate $ TJmp endIfLabel

                genNewBlock thenLabel [label]
                generateInstructions (createRT (dcReg RInt) (dcReg RFloat)) thenExpr

                -- There are no guarantee that the label has not been changed.
                currentThenLabel <- gets currentLabel
                terminate $ TJmp endIfLabel

                genNewBlock endIfLabel [currentThenLabel, currentElseLabel]
  where
    branchType = getType state
generateInstructions _ (Let _ (PRec _ _) _ _) =
    throwError $ UnexpectedError "The function should be removed during closure conversion."
generateInstructions outReg (Let _ PUnit expr body) = do
    generateInstructions outReg expr
    generateInstructions outReg body
generateInstructions outReg (Let state (PVar v) (Const _ (LInt 0)) body) = do
    reg <- lift $ genReg RInt v
    addInst $ IMov (getLoc state) reg (Reg $ zeroReg RInt)
    generateInstructions outReg body
generateInstructions outReg (Let state (PVar v) (Const _ (LFloat 0)) body) = do
    reg <- lift $ genReg RFloat v
    addInst $ IMov (getLoc state) reg (Reg $ zeroReg RFloat)
    generateInstructions outReg body
generateInstructions outReg (Let _ (PVar v) expr body) = do
    vTy <- lift $ liftB $ getTyOf v
    withRegType vTy $
        \case
            Just rTy -> do
                v' <- lift $ genReg rTy v
                generateInstructions (weakenReg v') expr
                generateInstructions outReg body
            Nothing -> do
                generateInstructions (createRT (dcReg RInt) (dcReg RFloat)) expr
                generateInstructions outReg body
generateInstructions outReg (Let state (PTuple vals) expr body) = do
    tuple <- lift $ genTempReg RInt
    generateInstructions (weakenReg tuple) expr
    store <-
        catMaybes
            <$> mapM
                ( \(idx, val) -> do
                    ty <- lift $ liftB $ getTyOf val
                    withRegType ty $
                        \case
                            Just rTy -> do
                                val' <- lift $ genReg rTy val
                                pure $ Just $ ILoad (getLoc state) val' tuple idx
                            Nothing -> pure Nothing
                )
                (zip [0 ..] vals)
    appendInst store
    generateInstructions outReg body
generateInstructions outReg (Var state (ExternalIdent ext)) = do
    -- Possibly a global variable.
    prop <- lift $ findGlobal ext
    let offset = globalOffset prop
    addInst $ IMov (getLoc state) (outReg #!! RInt) (Imm RInt offset)
generateInstructions outReg (Var state ident) = do
    ty <- lift $ liftB $ getTyOf ident
    withRegType ty $
        \case
            Just rTy -> do
                regID <- lift $ findReg rTy ident
                when (outReg #!! rTy /= regID) $
                    addInst $
                        IMov (getLoc state) (outReg #!! rTy) (Reg regID)
            Nothing -> do
                pure ()
generateInstructions outReg (Tuple state vars) = do
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
                                \case
                                    Just rTy -> do
                                        var' <- lift $ findReg rTy var
                                        pure [IStore (getLoc state) var' heapReg index]
                                    Nothing -> pure []
                )
                (zip [0 ..] vars)
    appendInst inst
    addInst $ IMov (getLoc state) (outReg #!! RInt) (Reg heapReg)
    addInst $ IIntOp (getLoc state) PAdd heapReg heapReg (Imm RInt $ length vars)
generateInstructions outReg (ArrayCreate state size _) = do
    constSize <- lift $ liftB $ asConstant size
    case constSize of
        (Just (LInt s)) -> do
            addInst $ IMov (getLoc state) (outReg #!! RInt) (Reg heapReg)
            addInst $ IIntOp (getLoc state) PAdd heapReg heapReg (Imm RInt s)
        _ -> do
            size' <- lift $ findReg RInt size
            addInst $ IMov (getLoc state) (outReg #!! RInt) (Reg heapReg)
            addInst $ IIntOp (getLoc state) PAdd heapReg heapReg (Reg size')
generateInstructions outReg (Get state array index) = do
    array' <- lift $ findRegOrImm RInt array
    constIndex <- lift $ liftB $ asConstant index
    case (array', constIndex) of
        (Imm _ offset, Just (LInt index')) -> do
            -- If both of them are constants, we can calculate the address at compile time.
            let ty = getType state
            withRegType ty $
                \case
                    Just rTy ->
                        addInst $ ILoad (getLoc state) (outReg #!! rTy) (zeroReg RInt) (offset + index')
                    Nothing -> pure ()
        (Imm _ offset, Nothing) -> do
            -- If the array is a constant and the index is a variable, we can calculate the address at compile time.
            index' <- lift $ findReg RInt index
            let ty = getType state
            withRegType ty $
                \case
                    Just rTy ->
                        addInst $ ILoad (getLoc state) (outReg #!! rTy) index' offset
                    Nothing -> pure ()
        (Reg reg, Just (LInt index')) -> do
            -- If the array is a register and the index is a constant, we can calculate the address at compile time.
            let ty = getType state
            withRegType ty $
                \case
                    Just rTy ->
                        addInst $ ILoad (getLoc state) (outReg #!! rTy) reg index'
                    Nothing -> pure ()
        _ -> do
            -- If both of them are variables, we need to calculate the address at runtime.
            index' <- lift $ findReg RInt index
            addr <- lift $ genTempReg RInt
            let ty = getType state
            withRegType ty $
                \case
                    Just rTy -> do
                        addInst $ IIntOp (getLoc state) PAdd addr index' array'
                        addInst $ ILoad (getLoc state) (outReg #!! rTy) addr 0
                    Nothing -> pure ()
generateInstructions _ (Put state dest idx src) = do
    dest' <- lift $ findRegOrImm RInt dest
    constIdx <- lift $ liftB $ asConstant idx

    srcTy <- lift $ liftB $ getTyOf src
    withRegType srcTy $
        \case
            Just rTy -> do
                src' <- lift $ findRegOrImm rTy src
                src'' <- resolveRegOrImm src'
                case (dest', constIdx) of
                    (Imm _ offset, Just (LInt index')) -> do
                        -- If both of them are constants, we can calculate the address at compile time.
                        addInst $ IStore (getLoc state) src'' (zeroReg RInt) (offset + index')
                    (Imm _ offset, Nothing) -> do
                        -- If the array is a constant and the index is a variable, we can calculate the address at compile time.
                        index' <- lift $ findReg RInt idx
                        addInst $ IStore (getLoc state) src'' index' offset
                    (Reg reg, Just (LInt index')) -> do
                        -- If the array is a register and the index is a constant, we can calculate the address at compile time.
                        addInst $ IStore (getLoc state) src'' reg index'
                    (Reg reg, Nothing) -> do
                        -- If both of them are variables, we need to calculate the address at runtime.
                        index' <- lift $ findReg RInt idx
                        addr <- lift $ genTempReg RInt
                        addInst $ IIntOp (getLoc state) PAdd addr reg (Reg index')
                        addInst $ IStore (getLoc state) src'' addr 0
                    _ -> throwError $ OtherError "The index should be integer."
            Nothing -> pure ()
generateInstructions outReg (MakeClosure state func freeV) = do
    -- Store the function label.
    labelReg <- lift $ genTempReg RInt
    addInst $ ILMov (getLoc state) labelReg (display func)
    addInst $ IStore (getLoc state) labelReg heapReg 0

    -- Store the free variables.
    freeV' <- findRegAll freeV
    mapM_
        ( \(v, offset) ->
            addInst $ IStore (getLoc state) v heapReg offset
        )
        $ zip (freeV' #!! RInt) [1 ..]
    mapM_
        ( \(v, offset) ->
            addInst $ IStore (getLoc state) v heapReg offset
        )
        $ zip (freeV' #!! RFloat) [1 + length (freeV' #!! RInt) ..]

    -- Get the address of the closure.
    let closureSize = 1 + length (freeV' #!! RInt) + length (freeV' #!! RFloat)
    addInst $ IMov (getLoc state) (outReg #!! RInt) (Reg heapReg)
    addInst $ IIntOp (getLoc state) PAdd heapReg heapReg (Imm RInt closureSize)
generateInstructions outReg (DirectApp state func args) = do
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
                                \case
                                    Just rTy -> do
                                        reg <- lift $ findRegOrImm rTy val
                                        reg' <- resolveRegOrImm reg
                                        addInst $ IStore (getLoc state) reg' (zeroReg RInt) (offset + idx)
                                    Nothing -> pure ()
                        )
                        ( zip
                            [0 ..]
                            values
                        )
                _ -> throwError $ OtherError "The first argument should be a tuple."
        else do
            -- A simple function application or a special instruction.
            args' <- findRegAll args

            case findBuiltin func of
                Just builtin -> do
                    -- A special instruction.
                    withRegType (getType state) $
                        \case
                            Just rTy ->
                                addInst $ IRawInst (getLoc state) (builtinInst builtin) (outReg #!! rTy) args'
                            Nothing ->
                                addInst $ IRawInst (getLoc state) (builtinInst builtin) (dcReg RInt) args'
                Nothing -> do
                    -- A simple function application.

                    -- Assign bounded function arguments.
                    eachRegType $
                        \rTy ->
                            mapM_
                                ( \(v, num) -> do
                                    addInst $ IMov (getLoc state) (argsReg rTy num) (Reg v)
                                )
                                $ zip (args' #!! rTy) [0 ..]

                    -- Call the function.
                    addInst $ ICall (getLoc state) (display func)

                    -- Assign the return value.
                    withRegType (getType state) $
                        \case
                            Just rTy ->
                                addInst $ IMov (getLoc state) (outReg #!! rTy) (Reg $ retReg rTy)
                            Nothing -> pure ()
generateInstructions outReg (ClosureApp state closure args) = do
    closure' <- lift $ findReg RInt closure
    func <- lift $ genTempReg RInt
    args' <- findRegAll args

    -- Assign bounded function arguments.
    eachRegType $
        \rTy ->
            mapM_
                ( \(v, num) -> do
                    addInst $ IMov (getLoc state) (argsReg rTy num) (Reg v)
                )
                $ zip (args' #!! rTy) [0 ..]

    -- Call the closure.
    addInst $ IIntOp (getLoc state) PAdd (argsReg RInt $ length (args' #!! RInt)) closure' (Imm RInt 1)
    addInst $ ILoad (getLoc state) func closure' 0
    addInst $ ICallReg (getLoc state) func

    -- Assign the return value.
    withRegType (getType state) $
        \case
            Just rTy ->
                addInst $ IMov (getLoc state) (outReg #!! rTy) (Reg $ retReg rTy)
            Nothing -> pure ()
generateInstructions outReg (Loop state args values body) = do
    prevLoopLabel <- gets loopLabel
    prevLoopArgs <- gets loopArgs

    current <- gets currentLabel

    name <- gets contextName
    labelID <- genNewLabelId
    let loopLabel' = name <> "_loop_" <> labelID

    args' <- lift $ genRegAll args
    values' <- findRegAll values

    -- To avoid different registers being merged by phi instructions,
    -- we need to create new registers.
    assigned <-
        buildRTM $
            \rTy ->
                mapM
                    ( \src -> do
                        dest <- lift $ genTempReg rTy
                        addInst $ IMov (getLoc state) dest (Reg src)
                        pure dest
                    )
                    $ values' #!! rTy

    modify $ \ctx ->
        ctx
            { loopLabel = loopLabel'
            , loopArgs = args'
            }

    terminate $ TJmp loopLabel'

    genNewBlock loopLabel' [current]

    eachRegType $
        \rTy ->
            mapM_ (\(a, b) -> addInst $ IPhi (getLoc state) a $ M.singleton current b) $
                zip (args' #!! rTy) ((assigned :: RegList) #!! rTy)

    generateInstructions outReg body

    modify $ \ctx ->
        ctx
            { loopLabel = prevLoopLabel
            , loopArgs = prevLoopArgs
            }
generateInstructions _ (Continue state args) = do
    loopReg <- gets loopArgs
    label <- gets loopLabel

    args' <- findRegAll args

    -- To avoid different registers being merged by phi instructions,
    -- we need to create new registers.
    assigned <-
        buildRTM $
            \rTy ->
                mapM
                    ( \src -> do
                        dest <- lift $ genTempReg rTy
                        addInst $ IMov (getLoc state) dest (Reg src)
                        pure dest
                    )
                    $ args' #!! rTy

    eachRegType $
        \rTy -> do
            mapM_ (uncurry addLoopPhi) $ zip (loopReg #!! rTy) ((assigned :: RegList) #!! rTy)

    terminate $ TJmp label

    genNewBlock "unreachable" []

generateCodeBlock ::
    (Monad m) =>
    Function ->
    CodeBlockStateT m ()
generateCodeBlock (Function _ _ _ freeVars boundedVars body) = do
    boundedVarsInstructions

    iArgsLength <- lift $ gets (argsLength . (#!! RInt) . regContext)
    freeVarsInstructions iArgsLength

    -- Generate function body.
    -- To prevent return registers from being merged by phi instructions,
    -- we need to create new registers.
    withRegType functionBodyTy $
        \case
            Just rTy -> do
                out <- lift $ genTempReg rTy
                generateInstructions (weakenReg out) body

                addInst $ IMov dummyLoc (retReg rTy) (Reg out)
            Nothing ->
                generateInstructions (createRT (dcReg RInt) (dcReg RFloat)) body

    terminate TReturn
  where
    functionBodyTy = getType $ getExprState body

    freeVarsInstructions :: (Monad m) => Int -> CodeBlockStateT m ()
    freeVarsInstructions closureArg = do
        freeRegisters <- lift $ genRegAll freeVars
        mapM_
            ( \(num, reg) ->
                addInst $ ILoad dummyLoc reg argReg num
            )
            $ zip [0 ..] (freeRegisters #!! RInt)
        mapM_
            ( \(num, reg) ->
                addInst $ ILoad dummyLoc reg argReg num
            )
            $ zip [length (freeRegisters #!! RInt) ..] (freeRegisters #!! RFloat)
      where
        argReg = argsReg RInt closureArg

    boundedVarsInstructions :: (Monad m) => CodeBlockStateT m ()
    boundedVarsInstructions = do
        boundedRegisters <- lift $ genRegAll boundedVars
        eachRegType $
            \rTy -> do
                lift $ updateRegContext rTy $ \ctx -> ctx{argsLength = length (boundedRegisters #!! rTy)}
                mapM_
                    ( \(num, reg) ->
                        addInst $ IMov dummyLoc reg (Reg (argsReg rTy num))
                    )
                    $ zip [0 ..] (boundedRegisters #!! rTy)

generateBlockGraph ::
    (Monad m) =>
    Function ->
    BackendIdentState m (BlockGraph VirtualInstKind)
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
                , loopArgs = mempty
                }
    let graph = BlockGraph (generatedBlocks ctx) funcLabel 0
    pure $ fillInPrevBlocks graph
  where
    funcLabel = display $ funcName func
