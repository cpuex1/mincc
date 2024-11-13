{-# LANGUAGE GADTs #-}

module Backend.FunctionCall (saveArgs) where

import Backend.Asm
import Backend.BackendEnv (BackendEnv (fArgsLen), BackendState, RegID, genTempFReg, genTempIReg, iArgsLen)
import Control.Monad.State.Lazy (MonadState (get, put), State, evalState, gets)
import Data.Foldable (foldlM)

saveArgs :: IntermediateCodeBlock stateTy RegID -> BackendState (IntermediateCodeBlock stateTy RegID)
saveArgs (IntermediateCodeBlock label inst) = do
    inst' <- saveArgs' inst
    pure $ IntermediateCodeBlock label inst'
  where
    saveArgs' :: [Inst stateTy RegID AllowBranch] -> BackendState [Inst stateTy RegID AllowBranch]
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
                [0 .. iLen]

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
            [0 .. fLen]

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
    isUsedAfterCallI reg (ICall _ _ : rest) = do
        put True
        isUsedAfterCallI reg rest
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
    isUsedAfterCallF reg (ICall _ _ : rest) = do
        put True
        isUsedAfterCallF reg rest
    isUsedAfterCallF reg (IFStore _ reg' _ _ : rest) = do
        called <- get
        rest' <- isUsedAfterCallF reg rest
        pure $ (called && reg == reg') || rest'
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
