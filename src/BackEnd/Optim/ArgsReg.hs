{-# LANGUAGE GADTs #-}

module BackEnd.Optim.ArgsReg (replaceSavedRegInGraph) where

import BackEnd.Optim.Common (BackEndOptimStateT)
import CodeBlock (BlockGraph (graphBlocks), CodeBlock (terminator), VirtualBlock, VirtualBlockGraph, visitInst)
import Control.Monad.State (MonadState (get), State, modify, runState)
import Data.Map (Map, fromList, insert, lookup, toList)
import IR (Inst (..), VirtualInst)
import Registers (
    RegID,
    RegMultiple,
    RegOrImm (Reg),
    RegReplaceable (mapReg),
    Register (Register),
    RegisterKind (ArgsReg, SavedReg),
    updateRT,
    (#!!),
 )
import Prelude hiding (lookup)

type ArgsRegState = State (RegMultiple (Map RegID RegID))

replaceArg :: RegMultiple (Map RegID RegID) -> Register a -> Register a
replaceArg replacement (Register rTy (SavedReg regID)) =
    case lookup regID (replacement #!! rTy) of
        Just arg -> Register rTy (ArgsReg arg)
        Nothing -> Register rTy (SavedReg regID)
replaceArg _ reg = reg

replaceSavedReg :: VirtualInst -> ArgsRegState VirtualInst
replaceSavedReg inst = do
    replacement <- get
    pure $ mapReg (replaceArg replacement) inst

removeArg :: Register a -> ArgsRegState ()
removeArg (Register rTy (ArgsReg arg)) = do
    modify (updateRT rTy (fromList . filter (\(_, a) -> a /= arg) . toList))
removeArg _ = pure ()

replaceSavedRegInGraph :: (Monad m) => VirtualBlockGraph -> BackEndOptimStateT m VirtualBlockGraph
replaceSavedRegInGraph graph =
    pure $ graph{graphBlocks = map replaceSavedRegInBlock (graphBlocks graph)}

replaceSavedRegInBlock :: VirtualBlock -> VirtualBlock
replaceSavedRegInBlock block =
    block'{terminator = mapReg (replaceArg state) (terminator block')}
  where
    (block', state) = runState (visitInst replaceSavedRegInInst block) mempty

replaceSavedRegInInst :: VirtualInst -> ArgsRegState VirtualInst
replaceSavedRegInInst inst@(IMov _ (Register rTy (SavedReg regID)) (Reg (Register _ (ArgsReg arg)))) = do
    modify (updateRT rTy (insert regID arg))
    pure inst
replaceSavedRegInInst inst@(ICall{}) = do
    modify (const mempty)
    pure inst
replaceSavedRegInInst inst@(ICallReg{}) = do
    modify (const mempty)
    pure inst
replaceSavedRegInInst inst@(IPhi{}) =
    pure inst
replaceSavedRegInInst inst@(ICompOp _ _ dest _ _) = do
    inst' <- replaceSavedReg inst
    removeArg dest
    pure inst'
replaceSavedRegInInst inst@(IIntOp _ _ dest _ _) = do
    inst' <- replaceSavedReg inst
    removeArg dest
    pure inst'
replaceSavedRegInInst inst@(IFOp _ _ dest _ _) = do
    inst' <- replaceSavedReg inst
    removeArg dest
    pure inst'
replaceSavedRegInInst inst@(IMov _ dest _) = do
    inst' <- replaceSavedReg inst
    removeArg dest
    pure inst'
replaceSavedRegInInst inst@(ILMov _ dest _) = do
    inst' <- replaceSavedReg inst
    removeArg dest
    pure inst'
replaceSavedRegInInst inst@(ILoad _ dest _ _) = do
    inst' <- replaceSavedReg inst
    removeArg dest
    pure inst'
replaceSavedRegInInst inst@(IStore{}) =
    replaceSavedReg inst
replaceSavedRegInInst inst@(IRawInst _ _ dest _) = do
    inst' <- replaceSavedReg inst
    removeArg dest
    pure inst'
