{-# LANGUAGE GADTs #-}

module Backend.FunctionCall (
    saveArgs,
    saveRegisters,
) where

import Backend.Asm
import Backend.BackendEnv (BackendEnv (fArgsLen), BackendStateT, genTempFReg, genTempIReg, iArgsLen)
import Backend.Liveness (LivenessLoc (LivenessLoc, livenessLoc), LivenessState (LivenessState), liveness)
import Control.Monad.State.Lazy (MonadState (get, put), State, evalState, gets)
import Data.Foldable (foldlM)
import Syntax (Loc, dummyLoc)

-- | Saves arguments on the stack before a function call.
saveArgs :: (Monad m) => IntermediateCodeBlock stateTy RegID -> BackendStateT m (IntermediateCodeBlock stateTy RegID)
saveArgs block = do
    inst' <- saveArgs' $ getICBInst block
    pure $ block{getICBInst = inst'}
  where
    saveArgs' :: (Monad m) => [Inst stateTy RegID AllowBranch] -> BackendStateT m [Inst stateTy RegID AllowBranch]
    saveArgs' inst'' = do
        iLen <- gets iArgsLen
        modInst <-
            foldlM
                ( \inst' arg -> do
                    if evalState (isUsedAfterCallI arg inst') False
                        then do
                            reg <- genTempIReg
                            -- The instructions must not be empty.
                            case inst' of
                                [] -> error "panic!"
                                (f : _) ->
                                    pure $ IMov (getIState f) reg (Reg (ArgsReg arg)) : map (replaceIReg (ArgsReg arg) reg) inst'
                        else
                            pure inst'
                )
                inst''
                [0 .. iLen - 1]

        fLen <- gets fArgsLen
        foldlM
            ( \inst' arg -> do
                if evalState (isUsedAfterCallF (ArgsReg arg) inst') False
                    then do
                        reg <- genTempFReg
                        -- The instructions must not be empty.
                        case inst' of
                            [] -> error "panic!"
                            (f : _) ->
                                pure $ IFMov (getIState f) reg (Reg (ArgsReg arg)) : map (replaceFReg (ArgsReg arg) reg) inst'
                    else
                        pure inst'
            )
            modInst
            [0 .. fLen - 1]

    isUsedAfterCallI :: Int -> [Inst stateTy RegID AllowBranch] -> State Bool Bool
    isUsedAfterCallI _ [] = pure False
    isUsedAfterCallI reg (ICompOp _ _ _ left right : rest) = do
        called <- get
        rest' <- isUsedAfterCallI reg rest
        pure $ (called && (ArgsReg reg == left || Reg (ArgsReg reg) == right)) || rest'
    isUsedAfterCallI reg (IIntOp _ _ _ left right : rest) = do
        called <- get
        rest' <- isUsedAfterCallI reg rest
        pure $ (called && (ArgsReg reg == left || Reg (ArgsReg reg) == right)) || rest'
    isUsedAfterCallI reg (IMov _ _ (Reg reg') : rest) = do
        called <- get
        rest' <- isUsedAfterCallI reg rest
        pure $ (called && (ArgsReg reg == reg')) || rest'
    isUsedAfterCallI reg (IRichCall _ _ args _ : rest) = do
        called <- get
        if called && elem (ArgsReg reg) args
            then
                pure True
            else do
                put True
                isUsedAfterCallI reg rest
    isUsedAfterCallI reg (IClosureCall _ _ args _ : rest) = do
        called <- get
        if called && elem (ArgsReg reg) args
            then
                pure True
            else do
                put True
                isUsedAfterCallI reg rest
    isUsedAfterCallI reg (IMakeClosure _ _ _ args _ : rest) = do
        called <- get
        rest' <- isUsedAfterCallI reg rest
        pure $ (called && elem (ArgsReg reg) args) || rest'
    isUsedAfterCallI reg (ILoad _ _ reg' _ : rest) = do
        called <- get
        rest' <- isUsedAfterCallI reg rest
        pure $ (called && (ArgsReg reg == reg')) || rest'
    isUsedAfterCallI reg (IStore _ reg1 reg2 _ : rest) = do
        called <- get
        rest' <- isUsedAfterCallI reg rest
        pure $ (called && (ArgsReg reg == reg1 || ArgsReg reg == reg2)) || rest'
    isUsedAfterCallI reg (IFLoad _ _ reg' _ : rest) = do
        called <- get
        rest' <- isUsedAfterCallI reg rest
        pure $ (called && (ArgsReg reg == reg')) || rest'
    isUsedAfterCallI reg (IFStore _ _ reg' _ : rest) = do
        called <- get
        rest' <- isUsedAfterCallI reg rest
        pure $ (called && ArgsReg reg == reg') || rest'
    isUsedAfterCallI reg (IRawInst _ _ _ iArgs _ : rest) = do
        called <- get
        rest' <- isUsedAfterCallI reg rest
        pure $ (called && (ArgsReg reg `elem` iArgs)) || rest'
    isUsedAfterCallI reg (IBranch _ _ left right thenBlock elseBlock : rest) = do
        called <- get
        if called && (ArgsReg reg == left || ArgsReg reg == right)
            then
                pure True
            else do
                thenBlock' <- isUsedAfterCallI reg thenBlock
                thenCalled <- get
                put called
                elseBlock' <- isUsedAfterCallI reg elseBlock
                elseCalled <- get
                if thenBlock' || elseBlock'
                    then
                        pure True
                    else do
                        put $ thenCalled || elseCalled
                        isUsedAfterCallI reg rest
    isUsedAfterCallI reg (_ : rest) = isUsedAfterCallI reg rest

    isUsedAfterCallF :: Register RegID Float -> [Inst stateTy RegID AllowBranch] -> State Bool Bool
    isUsedAfterCallF _ [] = pure False
    isUsedAfterCallF reg (IFCompOp _ _ _ left right : rest) = do
        called <- get
        rest' <- isUsedAfterCallF reg rest
        pure $ (called && (reg == left || reg == right)) || rest'
    isUsedAfterCallF reg (IFOp _ _ _ left right : rest) = do
        called <- get
        rest' <- isUsedAfterCallF reg rest
        pure $ (called && (reg == left || reg == right)) || rest'
    isUsedAfterCallF reg (IFMov _ _ (Reg reg') : rest) = do
        called <- get
        rest' <- isUsedAfterCallF reg rest
        pure $ (called && (reg == reg')) || rest'
    isUsedAfterCallF reg (IRichCall _ _ _ fArgs : rest) = do
        called <- get
        if called && elem reg fArgs
            then
                pure True
            else do
                put True
                isUsedAfterCallF reg rest
    isUsedAfterCallF reg (IClosureCall _ _ _ fArgs : rest) = do
        called <- get
        if called && elem reg fArgs
            then
                pure True
            else do
                put True
                isUsedAfterCallF reg rest
    isUsedAfterCallF reg (IMakeClosure _ _ _ _ fArgs : rest) = do
        called <- get
        rest' <- isUsedAfterCallF reg rest
        pure $ (called && elem reg fArgs) || rest'
    isUsedAfterCallF reg (IFStore _ reg' _ _ : rest) = do
        called <- get
        rest' <- isUsedAfterCallF reg rest
        pure $ (called && reg == reg') || rest'
    isUsedAfterCallF reg (IRawInst _ _ _ _ fArgs : rest) = do
        called <- get
        rest' <- isUsedAfterCallF reg rest
        pure $ (called && (reg `elem` fArgs)) || rest'
    isUsedAfterCallF reg (IBranch _ _ _ _ thenBlock elseBlock : rest) = do
        called <- get
        thenBlock' <- isUsedAfterCallF reg thenBlock
        thenCalled <- get
        put called
        elseBlock' <- isUsedAfterCallF reg elseBlock
        elseCalled <- get
        if thenBlock' || elseBlock'
            then
                pure True
            else do
                put $ thenCalled || elseCalled
                isUsedAfterCallF reg rest
    isUsedAfterCallF reg (_ : rest) = isUsedAfterCallF reg rest

-- | Saves registers on the stack before a function call and restores them after the call.
saveRegBeyondCall :: Inst LivenessLoc RegID AllowBranch -> [Inst Loc RegID AllowBranch]
saveRegBeyondCall (IRichCall (LivenessLoc loc (LivenessState iArgs' fArgs')) label iArgs fArgs) =
    prologue ++ [IRichCall loc label iArgs fArgs] ++ epilogue
  where
    iToBeSaved = iArgs'
    fToBeSaved = fArgs'

    prologue =
        if null iToBeSaved && null fToBeSaved
            then
                []
            else
                IIntOp dummyLoc PAdd StackReg StackReg (Imm $ -(4 * (length iToBeSaved + length fToBeSaved))) : (iPrologue ++ fPrologue)
    iPrologue =
        zipWith
            (\i arg -> IStore dummyLoc (SavedReg arg) StackReg (i * 4))
            [0 ..]
            iToBeSaved
    fPrologue =
        zipWith
            (\i arg -> IFStore dummyLoc (SavedReg arg) StackReg (i * 4))
            [length iToBeSaved ..]
            fToBeSaved

    epilogue =
        if null iToBeSaved && null fToBeSaved
            then
                []
            else
                iEpilogue ++ fEpilogue ++ [IIntOp dummyLoc PAdd StackReg StackReg (Imm $ 4 * (length iToBeSaved + length fToBeSaved))]
    iEpilogue =
        zipWith
            (\i arg -> ILoad dummyLoc (SavedReg arg) StackReg (i * 4))
            [0 ..]
            iToBeSaved
    fEpilogue =
        zipWith
            (\i arg -> IFLoad dummyLoc (SavedReg arg) StackReg (i * 4))
            [length iToBeSaved ..]
            fToBeSaved
saveRegBeyondCall (IClosureCall (LivenessLoc loc (LivenessState iArgs' fArgs')) cl iArgs fArgs) =
    prologue ++ [IClosureCall loc cl iArgs fArgs] ++ epilogue
  where
    iToBeSaved = iArgs'
    fToBeSaved = fArgs'

    prologue =
        if null iToBeSaved && null fToBeSaved
            then
                []
            else
                IIntOp dummyLoc PAdd StackReg StackReg (Imm $ -(4 * (length iToBeSaved + length fToBeSaved))) : (iPrologue ++ fPrologue)
    iPrologue =
        zipWith
            (\i arg -> IStore dummyLoc (SavedReg arg) StackReg (i * 4))
            [0 ..]
            iToBeSaved
    fPrologue =
        zipWith
            (\i arg -> IFStore dummyLoc (SavedReg arg) StackReg (i * 4))
            [length iToBeSaved ..]
            fToBeSaved

    epilogue =
        if null iToBeSaved && null fToBeSaved
            then
                []
            else
                iEpilogue ++ fEpilogue ++ [IIntOp dummyLoc PAdd StackReg StackReg (Imm $ 4 * (length iToBeSaved + length fToBeSaved))]
    iEpilogue =
        zipWith
            (\i arg -> ILoad dummyLoc (SavedReg arg) StackReg (i * 4))
            [0 ..]
            iToBeSaved
    fEpilogue =
        zipWith
            (\i arg -> IFLoad dummyLoc (SavedReg arg) StackReg (i * 4))
            [length iToBeSaved ..]
            fToBeSaved
saveRegBeyondCall (IBranch state op left right thenBlock elseBlock) =
    [ IBranch
        (livenessLoc state)
        op
        left
        right
        (concatMap saveRegBeyondCall thenBlock)
        (concatMap saveRegBeyondCall elseBlock)
    ]
saveRegBeyondCall i = [substIState livenessLoc i]

-- | Saves registers on the stack before a function call and restores them after the call.
saveRegisters :: IntermediateCodeBlock Loc RegID -> IntermediateCodeBlock Loc RegID
saveRegisters block =
    block{getICBInst = concatMap saveRegBeyondCall $ liveness $ getICBInst block}
