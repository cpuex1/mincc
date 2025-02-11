{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module CodeBlock (
    CodeBlock (..),
    BlockGraph (..),
    Terminator (..),
    nextBlocks,
    lookupBlock,
) where

import Data.Text (intercalate)
import Display (Display (display), DisplayI (displayI))
import IR (Inst, InstKind (InstStateTy, RegIDTy), InstLabel)
import Registers (RegID, RegType (RFloat, RInt), Register (Register))
import Syntax (RelationBinOp (..))

-- | The last instruction of a block
data Terminator ty where
    TJmp :: InstLabel -> Terminator ty
    TBranch ::
        RelationBinOp ->
        Register RegID a ->
        Register RegID a ->
        InstLabel ->
        InstLabel ->
        Terminator ty
    TReturn :: Terminator ty

instance Display (Terminator ty) where
    display (TJmp l) = "jmp " <> l
    display (TBranch op r1 r2 l1 l2) =
        "b" <> floatPrefix <> opPrefix <> " " <> display r1 <> ", " <> display r2 <> ", " <> l1 <> "\njmp " <> l2
      where
        opPrefix = case op of
            Eq -> "eq"
            Ne -> "ne"
            Lt -> "lt"
            Ge -> "ge"
        floatPrefix = case r1 of
            Register RFloat _ -> "f"
            Register RInt _ -> ""
    display TReturn = "ret"

-- | A block that holds instructions and a terminator
data CodeBlock ty = CodeBlock
    { blockName :: InstLabel
    , blockInst :: [Inst ty]
    , prevBlocks :: [InstLabel]
    , terminator :: Terminator ty
    }

instance (Display (InstStateTy ty), RegIDTy ty ~ RegID) => Display (CodeBlock ty) where
    display (CodeBlock l inst _ term) =
        l <> ":" <> intercalate "" (map (\i -> "\n" <> displayI 1 i) inst) <> display term

-- | Get the next blocks from a block
nextBlocks :: CodeBlock ty -> [InstLabel]
nextBlocks (CodeBlock _ _ _ TReturn) = []
nextBlocks (CodeBlock _ _ _ (TJmp l)) = [l]
nextBlocks (CodeBlock _ _ _ (TBranch _ _ _ l1 l2)) = [l1, l2]

data BlockGraph ty = BlockGraph
    { blocks :: [CodeBlock ty]
    , entryBlock :: InstLabel
    }

instance (Display (InstStateTy ty), RegIDTy ty ~ RegID) => Display (BlockGraph ty) where
    display (BlockGraph b _) = intercalate "\n" $ map display b

lookupBlock :: InstLabel -> BlockGraph ty -> Maybe (CodeBlock ty)
lookupBlock l block = lookup l $ map (\b -> (blockName b, b)) $ blocks block
