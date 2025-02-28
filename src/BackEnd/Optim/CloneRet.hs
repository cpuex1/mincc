{-# LANGUAGE OverloadedStrings #-}

module BackEnd.Optim.CloneRet (cloneRet) where

import BackEnd.Analysis.CodeBlock (fillInPrevBlocks)
import BackEnd.Analysis.IR (InOutSet (outSet), inOutRegisters)
import BackEnd.BackendEnv (genFreshID, genTempReg)
import BackEnd.Optim.Common (BackEndOptimStateT, updatePhi)
import CodeBlock (
    BlockGraph (graphBlocks),
    CodeBlock (CodeBlock, blockName, terminator),
    Terminator (TJmp, TReturn),
    VirtualBlock,
    VirtualBlockGraph,
    lookupBlock,
    visitBlock,
    visitInst,
 )
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.State (MonadState (get), MonadTrans (lift), StateT, evalStateT, modify)
import Data.Foldable (Foldable (toList))
import Data.Map (fromList)
import Data.Text (pack)
import Error (CompilerError (UnexpectedError))
import IR (VirtualInst)
import Registers (RegMapping (RegMapping), RegMultiple, Register (Register), RegisterKind (SavedReg), applyRegMapping, buildRTM, (#!!))

type CloneRetStateT m = StateT (RegMultiple RegMapping) (BackEndOptimStateT m)

cloneBlock :: (Monad m) => VirtualBlock -> BackEndOptimStateT m VirtualBlock
cloneBlock block = do
    block' <- evalStateT (visitInst cloneInst block) mempty
    fresh <- lift genFreshID
    pure $ block'{blockName = blockName block <> "_cloned_" <> pack (show fresh)}

cloneInst :: (Monad m) => VirtualInst -> CloneRetStateT m VirtualInst
cloneInst inst = do
    let outS = outSet <$> inOutRegisters inst
    newMapping <-
        buildRTM
            ( \rTy -> do
                mapped <-
                    mapM
                        ( \o -> do
                            reg <- lift $ lift $ genTempReg rTy
                            case reg of
                                (Register _ (SavedReg regID)) ->
                                    pure (o, regID)
                                _ -> throwError $ UnexpectedError "The generated register should be a saved register."
                        )
                        (toList $ outS #!! rTy)
                pure $ RegMapping $ fromList mapped
            )
    modify (newMapping <>)
    mapping <- get
    pure $ applyRegMapping mapping inst

cloneRet :: (Monad m) => VirtualBlockGraph -> BackEndOptimStateT m VirtualBlockGraph
cloneRet graph = do
    cloned <- cloneRetBlock $ graphBlocks graph
    visitBlock (pure . updatePhi) $ fillInPrevBlocks $ graph{graphBlocks = cloned}

cloneRetBlock :: (Monad m) => [VirtualBlock] -> BackEndOptimStateT m [VirtualBlock]
cloneRetBlock [] = pure []
cloneRetBlock (block@(CodeBlock _ _ _ (TJmp next)) : blocks) = do
    case lookupBlock next blocks of
        Just nextBlock@(CodeBlock _ _ prev TReturn) -> do
            if length prev <= 1
                then
                    (block :) <$> cloneRetBlock (nextBlock : blocks)
                else do
                    newBlock <- cloneBlock nextBlock
                    let modified = block{terminator = TJmp (blockName newBlock)}
                    ([modified, newBlock] ++) <$> cloneRetBlock blocks
        _ -> (block :) <$> cloneRetBlock blocks
cloneRetBlock (block : blocks) = (block :) <$> cloneRetBlock blocks
