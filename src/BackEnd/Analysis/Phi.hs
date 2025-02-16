{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module BackEnd.Analysis.Phi (
    PhiGroups (..),
    addGroup,
    phiGroups,
    toPhiMapping,
    applyMapping,
    applyMappingToLiveness,
) where

import BackEnd.Liveness (Liveness (Liveness))
import CodeBlock (BlockGraph, visitBlock, visitInst)
import Control.Monad.State (MonadState (get, put), State, execState)
import Data.Map (Map, elems, insert, lookup)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set, disjoint, fromList, lookupMin, union)
import qualified Data.Set as S
import IR (Inst (IPhi))
import Registers (
    RegID,
    RegType,
    RegVariant (RegVariant),
    RegVariant',
    Register (Register),
    RegisterKind (SavedReg),
    VariantItem (VariantItem),
    updateVariant,
    (#!!),
    (#$),
 )
import Prelude hiding (lookup)

--
-- In this compiler, we call a phi group a set of registers that should be merged into a single register.
-- To calculate the phi groups, we should visit and analyze the phi instructions in the block graph.
--

newtype PhiGroups a
    = PhiGroups [Set RegID]
    deriving (Show, Eq)

addGroup :: Set RegID -> PhiGroups a -> PhiGroups a
addGroup reg (PhiGroups []) = PhiGroups [reg]
addGroup reg (PhiGroups (group : remains)) =
    if disjoint reg group
        then
            let PhiGroups merged = addGroup reg (PhiGroups remains)
             in PhiGroups (group : merged)
        else addGroup (reg `union` group) (PhiGroups remains)

newtype PhiMapping a = PhiMapping {pMapping :: Map RegID RegID}
    deriving (Show, Eq)

-- | Convert the phi groups to a mapping from the registers to the single register.
toPhiMapping :: RegVariant PhiGroups -> RegVariant PhiMapping
toPhiMapping groups =
    ( \_ (PhiGroups group) ->
        PhiMapping $
            foldr
                ( \group' mapping ->
                    case lookupMin group' of
                        Just dest -> foldr (`insert` dest) mapping group'
                        Nothing -> mapping
                )
                mempty
                group
    )
        #$ groups

-- | Apply the phi mapping to a register.
applyMapping :: RegType a -> RegVariant PhiMapping -> RegID -> RegID
applyMapping rTy mapping reg =
    fromMaybe reg $ lookup reg $ pMapping (mapping #!! rTy)

-- | Apply the phi mapping to the liveness information.
applyMappingToLiveness :: RegVariant PhiMapping -> RegVariant Liveness -> RegVariant Liveness
applyMappingToLiveness mapping live =
    (\rTy (Liveness live') -> Liveness $ S.map (applyMapping rTy mapping) live') #$ live

type GroupState = State (RegVariant PhiGroups)

registerGroup :: RegVariant' (Set RegID) -> GroupState ()
registerGroup (RegVariant (VariantItem iGroup) (VariantItem fGroup)) = do
    (RegVariant iGroups fGroups) <- get
    put $ RegVariant (addGroup iGroup iGroups) (addGroup fGroup fGroups)

-- | Calculate the phi groups in the block graph
phiGroups :: BlockGraph ty -> RegVariant PhiGroups
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
    emptyGroups = RegVariant (PhiGroups []) (PhiGroups [])

    toGroup :: Inst ty -> RegVariant' (Set RegID)
    toGroup (IPhi _ (Register rTy (SavedReg dest)) srcs) =
        updateVariant rTy (\_ -> VariantItem srcRegIDs) mempty
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
