{-# LANGUAGE GADTs #-}

module BackEnd.Analysis.IR (
    inOutRegisters,
    InOutSet (..),
) where

import Data.Map (elems)
import Data.Set (Set, insert)
import IR (Inst (..))
import Registers (
    RegID,
    RegMultiple,
    RegOrImm (Reg),
    Register (Register),
    RegisterKind (SavedReg),
    buildRT,
    updateRT,
 )

data InOutSet
    = InOutSet
    { inSet :: Set RegID
    , outSet :: Set RegID
    }

emptyInOutSet :: InOutSet
emptyInOutSet = InOutSet{inSet = mempty, outSet = mempty}

insertInReg :: Register a -> RegMultiple InOutSet -> RegMultiple InOutSet
insertInReg (Register rTy (SavedReg regID)) =
    updateRT rTy (\set -> set{inSet = insert regID (inSet set)})
insertInReg _ = id

insertInRegOrImm :: RegOrImm a -> RegMultiple InOutSet -> RegMultiple InOutSet
insertInRegOrImm (Reg reg) = insertInReg reg
insertInRegOrImm _ = id

insertOutReg :: Register a -> RegMultiple InOutSet -> RegMultiple InOutSet
insertOutReg (Register rTy (SavedReg regID)) =
    updateRT rTy (\set -> set{outSet = insert regID (outSet set)})
insertOutReg _ = id

-- | Compute the set of registers that are used as input and output for an instruction.
inOutRegisters :: Inst ty -> RegMultiple InOutSet
inOutRegisters (ICompOp _ _ dest lhs rhs) =
    insertOutReg dest
        . insertInReg lhs
        . insertInRegOrImm rhs
        $ buildRT (const emptyInOutSet)
inOutRegisters (IIntOp _ _ dest lhs rhs) =
    insertOutReg dest
        . insertInReg lhs
        . insertInRegOrImm rhs
        $ buildRT (const emptyInOutSet)
inOutRegisters (IFOp _ _ dest lhs rhs) =
    insertOutReg dest
        . insertInReg lhs
        . insertInReg rhs
        $ buildRT (const emptyInOutSet)
inOutRegisters (IMov _ dest src) =
    insertOutReg dest
        . insertInRegOrImm src
        $ buildRT (const emptyInOutSet)
inOutRegisters (ICall _ _) =
    buildRT (const emptyInOutSet)
inOutRegisters (ILMov _ dest _) =
    insertOutReg dest $
        buildRT (const emptyInOutSet)
inOutRegisters (ICallReg _ cl) =
    insertInReg cl $
        buildRT (const emptyInOutSet)
inOutRegisters (ILoad _ dest src _) =
    insertOutReg dest
        . insertInReg src
        $ buildRT (const emptyInOutSet)
inOutRegisters (IStore _ dest src _) =
    insertInReg dest
        . insertInReg src
        $ buildRT (const emptyInOutSet)
inOutRegisters (IRawInst _ _ retTy iArgs fArgs) =
    let set = buildRT (const emptyInOutSet)
     in let set' = foldl (flip insertInRegOrImm) set iArgs
         in let set'' = foldl (flip insertInReg) set' fArgs
             in insertOutReg retTy set''
inOutRegisters (IPhi _ dest srcs) =
    let set = buildRT (const emptyInOutSet)
     in let set' = foldl (flip insertInReg) set $ elems srcs
         in insertOutReg dest set'
