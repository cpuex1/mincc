{-# LANGUAGE GADTs #-}

module BackEnd.FunctionCall (
    saveRegisters,
) where

import BackEnd.Liveness (LivenessLoc (LivenessLoc, livenessLoc), LivenessState (LivenessState), liveness)
import Data.Set (toAscList)
import IR (
    AllowBranch,
    Inst (
        IBranch,
        IClosureCall,
        IFLoad,
        IFStore,
        IIntOp,
        ILoad,
        IRichCall,
        IStore
    ),
    IntermediateCodeBlock (getICBInst),
    PrimitiveIntOp (PAdd),
    RegID,
    substIState,
 )
import Registers (RegOrImm (Imm), RegType (RFloat, RInt), savedReg, stackReg)
import Syntax (Loc, dummyLoc)

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
                IIntOp dummyLoc PAdd stackReg stackReg (Imm $ -(length iToBeSaved + length fToBeSaved)) : (iPrologue ++ fPrologue)
    iPrologue =
        zipWith
            (\i arg -> IStore dummyLoc (savedReg RInt arg) stackReg i)
            [0 ..]
            $ toAscList iToBeSaved
    fPrologue =
        zipWith
            (\i arg -> IFStore dummyLoc (savedReg RFloat arg) stackReg i)
            [length iToBeSaved ..]
            $ toAscList fToBeSaved

    epilogue =
        if null iToBeSaved && null fToBeSaved
            then
                []
            else
                iEpilogue ++ fEpilogue ++ [IIntOp dummyLoc PAdd stackReg stackReg (Imm $ length iToBeSaved + length fToBeSaved)]
    iEpilogue =
        zipWith
            (\i arg -> ILoad dummyLoc (savedReg RInt arg) stackReg i)
            [0 ..]
            $ toAscList iToBeSaved
    fEpilogue =
        zipWith
            (\i arg -> IFLoad dummyLoc (savedReg RFloat arg) stackReg i)
            [length iToBeSaved ..]
            $ toAscList fToBeSaved
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
                IIntOp dummyLoc PAdd stackReg stackReg (Imm $ -(length iToBeSaved + length fToBeSaved)) : (iPrologue ++ fPrologue)
    iPrologue =
        zipWith
            (\i arg -> IStore dummyLoc (savedReg RInt arg) stackReg i)
            [0 ..]
            $ toAscList iToBeSaved
    fPrologue =
        zipWith
            (\i arg -> IFStore dummyLoc (savedReg RFloat arg) stackReg i)
            [length iToBeSaved ..]
            $ toAscList fToBeSaved

    epilogue =
        if null iToBeSaved && null fToBeSaved
            then
                []
            else
                iEpilogue ++ fEpilogue ++ [IIntOp dummyLoc PAdd stackReg stackReg (Imm $ length iToBeSaved + length fToBeSaved)]
    iEpilogue =
        zipWith
            (\i arg -> ILoad dummyLoc (savedReg RInt arg) stackReg i)
            [0 ..]
            $ toAscList iToBeSaved
    fEpilogue =
        zipWith
            (\i arg -> IFLoad dummyLoc (savedReg RFloat arg) stackReg i)
            [length iToBeSaved ..]
            $ toAscList fToBeSaved
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
