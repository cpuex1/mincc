{-# LANGUAGE GADTs #-}

module BackEnd.Analysis.IR (
    inOutRegisters,
    inOutRegisters',
    InOutSet (..),
    InOutSet' (..),
) where

import Data.Map (elems)
import Data.Set (Set, insert)
import IR (Inst (..))
import Registers (
    RegID,
    RegMultiple,
    RegOrImm (Reg),
    RegType (RFloat, RInt),
    RegVariant,
    Register (Register),
    RegisterKind (SavedReg),
    buildRT,
    updateRT,
    (#!!),
    (#$),
 )

data InOutSet' rTy
    = InOutSet'
    { inSet' :: Set (Register rTy)
    , outSet' :: Set (Register rTy)
    }
    deriving (Show, Eq)

data InOutSet
    = InOutSet
    { inSet :: Set RegID
    , outSet :: Set RegID
    }

convertInOut :: InOutSet' rTy -> InOutSet
convertInOut (InOutSet' i o) =
    InOutSet
        { inSet =
            foldr
                ( maybe id insert . getRegID
                )
                mempty
                i
        , outSet =
            foldr
                ( maybe id insert . getRegID
                )
                mempty
                o
        }
  where
    getRegID :: Register a -> Maybe RegID
    getRegID (Register _ (SavedReg regID)) = Just regID
    getRegID _ = Nothing

emptyInOutSet :: InOutSet' rTy
emptyInOutSet = InOutSet'{inSet' = mempty, outSet' = mempty}

insertInReg :: Register a -> RegVariant InOutSet' -> RegVariant InOutSet'
insertInReg reg@(Register rTy _) =
    updateRT rTy (\set -> set{inSet' = insert reg (inSet' set)})

insertInRegOrImm :: RegOrImm a -> RegVariant InOutSet' -> RegVariant InOutSet'
insertInRegOrImm (Reg reg) = insertInReg reg
insertInRegOrImm _ = id

insertOutReg :: Register a -> RegVariant InOutSet' -> RegVariant InOutSet'
insertOutReg reg@(Register rTy _) =
    updateRT rTy (\set -> set{outSet' = insert reg (outSet' set)})

-- | Compute the set of registers that are used as input and output for an instruction.
inOutRegisters :: Inst ty -> RegMultiple InOutSet
inOutRegisters inst = const convertInOut #$ inOutRegisters' inst

-- | Compute the set of registers that are used as input and output for an instruction.
inOutRegisters' :: Inst ty -> RegVariant InOutSet'
inOutRegisters' (ICompOp _ _ dest lhs rhs) =
    insertOutReg dest
        . insertInReg lhs
        . insertInRegOrImm rhs
        $ buildRT (const emptyInOutSet)
inOutRegisters' (IIntOp _ _ dest lhs rhs) =
    insertOutReg dest
        . insertInReg lhs
        . insertInRegOrImm rhs
        $ buildRT (const emptyInOutSet)
inOutRegisters' (IFOp _ _ dest lhs rhs) =
    insertOutReg dest
        . insertInReg lhs
        . insertInReg rhs
        $ buildRT (const emptyInOutSet)
inOutRegisters' (IMov _ dest src) =
    insertOutReg dest
        . insertInRegOrImm src
        $ buildRT (const emptyInOutSet)
inOutRegisters' (ICall _ _) =
    buildRT (const emptyInOutSet)
inOutRegisters' (ILMov _ dest _) =
    insertOutReg dest $
        buildRT (const emptyInOutSet)
inOutRegisters' (ICallReg _ cl) =
    insertInReg cl $
        buildRT (const emptyInOutSet)
inOutRegisters' (ILoad _ dest src _) =
    insertOutReg dest
        . insertInReg src
        $ buildRT (const emptyInOutSet)
inOutRegisters' (IStore _ dest src _) =
    insertInReg dest
        . insertInReg src
        $ buildRT (const emptyInOutSet)
inOutRegisters' (IRawInst _ _ retTy args) =
    let set = buildRT (const emptyInOutSet)
     in let set' = foldl (flip insertInReg) set (args #!! RInt)
         in let set'' = foldl (flip insertInReg) set' (args #!! RFloat)
             in insertOutReg retTy set''
inOutRegisters' (IPhi _ dest srcs) =
    let set = buildRT (const emptyInOutSet)
     in let set' = foldl (flip insertInReg) set $ elems srcs
         in insertOutReg dest set'
