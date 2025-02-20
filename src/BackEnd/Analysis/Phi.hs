{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module BackEnd.Analysis.Phi (
    PhiGroups (..),
    addGroup,
    phiGroups,
    toPhiMapping,
) where

import CodeBlock (BlockGraph, visitBlock, visitInst)
import Control.Monad.State (State, execState, modify)
import Data.Map (elems, insert)
import Data.Maybe (mapMaybe)
import Data.Set (Set, disjoint, fromList, lookupMin, union)
import IR (Inst (IPhi))
import Registers (
    RegID,
    RegMapping (RegMapping),
    RegMultiple,
    RegTuple (createRT),
    Register (Register),
    RegisterKind (SavedReg),
    updateRT,
    (#!!),
    (#$),
 )
import Prelude hiding (lookup)

--
-- In this compiler, we call a phi group a set of registers that should be merged into a single register.
-- To calculate the phi groups, we should visit and analyze the phi instructions in the block graph.
--

newtype PhiGroups
    = PhiGroups [Set RegID]
    deriving (Show, Eq)

addGroup :: Set RegID -> PhiGroups -> PhiGroups
addGroup reg (PhiGroups []) = PhiGroups [reg]
addGroup reg (PhiGroups (group : remains)) =
    if disjoint reg group
        then
            let PhiGroups merged = addGroup reg (PhiGroups remains)
             in PhiGroups (group : merged)
        else addGroup (reg `union` group) (PhiGroups remains)

-- | Convert the phi groups to a mapping from the registers to the single register.
toPhiMapping :: RegMultiple PhiGroups -> RegMultiple RegMapping
toPhiMapping groups =
    ( \_ (PhiGroups group) ->
        RegMapping $
            foldr
                ( \group' mapping ->
                    case lookupMin group' of
                        Just minReg ->
                            -- Maps registers to the one with the minimum register ID.
                            foldr (`insert` minReg) mapping group'
                        Nothing -> mapping
                )
                mempty
                group
    )
        #$ groups

type GroupState = State (RegMultiple PhiGroups)

registerGroup :: RegMultiple (Set RegID) -> GroupState ()
registerGroup group = do
    modify $ \groups -> (\rTy groups' -> addGroup (group #!! rTy) groups') #$ groups

-- | Calculate the phi groups in the block graph
phiGroups :: BlockGraph ty -> RegMultiple PhiGroups
phiGroups graph =
    execState
        ( visitBlock
            ( visitInst
                ( \inst -> do
                    registerGroup (toGroup inst)
                    pure inst
                )
            )
            graph
        )
        emptyGroups
  where
    emptyGroups = createRT (PhiGroups []) (PhiGroups [])

    toGroup :: Inst ty -> RegMultiple (Set RegID)
    toGroup (IPhi _ (Register rTy (SavedReg dest)) srcs) =
        updateRT rTy (const srcRegIDs) mempty
      where
        srcs' = elems srcs
        srcRegIDs =
            fromList $
                dest
                    : mapMaybe
                        ( \case
                            Register _ (SavedReg r) -> Just r
                            _ -> Nothing
                        )
                        srcs'
    toGroup _ = mempty
