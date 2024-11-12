{-# LANGUAGE GADTs #-}

module Backend.FunctionCall (saveArgs) where

import Backend.Asm
import Backend.BackendEnv (BackendState, RegID, genTempIReg, iArgsLen)
import Control.Monad.State.Lazy (MonadState (get, put), State, evalState, gets)
import Data.Foldable (foldlM)

saveArgs :: IntermediateCodeBlock stateTy Int -> BackendState (IntermediateCodeBlock stateTy Int)
saveArgs (IntermediateCodeBlock label inst) = do
    inst' <- saveArgs' inst
    pure $ IntermediateCodeBlock label inst'
  where
    saveArgs' :: [Inst stateTy RegID AllowBranch] -> BackendState [Inst stateTy RegID AllowBranch]
    saveArgs' inst'' = do
        argLen <- gets iArgsLen
        foldlM
            ( \inst' arg -> do
                if evalState (isUsedAfterCall arg inst') False
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
            [0 .. argLen]

    isUsedAfterCall :: Int -> [Inst stateTy RegID AllowBranch] -> State Bool Bool
    isUsedAfterCall _ [] = pure False
    isUsedAfterCall reg (ICompOp _ _ _ left (Imm _) : rest) = do
        called <- get
        rest' <- isUsedAfterCall reg rest
        pure $ (called && ArgsReg reg == left) || rest'
    isUsedAfterCall reg (ICompOp _ _ _ left (Reg right) : rest) = do
        called <- get
        rest' <- isUsedAfterCall reg rest
        pure $ (called && (ArgsReg reg == left || ArgsReg reg == right)) || rest'
    isUsedAfterCall reg (IIntOp _ _ _ left (Imm _) : rest) = do
        called <- get
        rest' <- isUsedAfterCall reg rest
        pure $ (called && ArgsReg reg == left) || rest'
    isUsedAfterCall reg (IIntOp _ _ _ left (Reg right) : rest) = do
        called <- get
        rest' <- isUsedAfterCall reg rest
        pure $ (called && (ArgsReg reg == left || ArgsReg reg == right)) || rest'
    isUsedAfterCall reg (IMov _ _ (Reg reg') : rest) = do
        called <- get
        rest' <- isUsedAfterCall reg rest
        pure $ (called && (ArgsReg reg == reg')) || rest'
    isUsedAfterCall reg (IRichCall _ _ args _ : rest) = do
        called <- get
        if called && elem (ArgsReg reg) args
            then
                pure True
            else do
                put True
                isUsedAfterCall reg rest
    isUsedAfterCall reg (IClosureCall _ _ args _ : rest) = do
        called <- get
        if called && elem (ArgsReg reg) args
            then
                pure True
            else do
                put True
                isUsedAfterCall reg rest
    isUsedAfterCall reg (IMakeClosure _ _ _ args _ : rest) = do
        called <- get
        rest' <- isUsedAfterCall reg rest
        pure $ (called && elem (ArgsReg reg) args) || rest'
    isUsedAfterCall reg (ICall _ _ : rest) = do
        put True
        isUsedAfterCall reg rest
    isUsedAfterCall reg (ILoad _ _ reg' _ : rest) = do
        called <- get
        rest' <- isUsedAfterCall reg rest
        pure $ (called && (ArgsReg reg == reg')) || rest'
    isUsedAfterCall reg (IStore _ reg1 reg2 _ : rest) = do
        called <- get
        rest' <- isUsedAfterCall reg rest
        pure $ (called && (ArgsReg reg == reg1 || ArgsReg reg == reg2)) || rest'
    isUsedAfterCall reg (IFLoad _ _ reg' _ : rest) = do
        called <- get
        rest' <- isUsedAfterCall reg rest
        pure $ (called && (ArgsReg reg == reg')) || rest'
    isUsedAfterCall reg (IFStore _ _ reg' _ : rest) = do
        called <- get
        rest' <- isUsedAfterCall reg rest
        pure $ (called && ArgsReg reg == reg') || rest'
    isUsedAfterCall reg (IBranch _ _ left right thenBlock elseBlock : rest) = do
        called <- get
        if called && (ArgsReg reg == left || ArgsReg reg == right)
            then
                pure True
            else do
                thenBlock' <- isUsedAfterCall reg thenBlock
                thenCalled <- get
                put called
                elseBlock' <- isUsedAfterCall reg elseBlock
                elseCalled <- get
                if thenBlock' || elseBlock'
                    then
                        pure True
                    else do
                        put $ thenCalled || elseCalled
                        isUsedAfterCall reg rest
    isUsedAfterCall reg (_ : rest) = isUsedAfterCall reg rest
