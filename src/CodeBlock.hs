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
    asMermaidGraph,
) where

import Data.Text (Text, intercalate)
import Display (Display (display), DisplayI (displayI), insertIndent)
import IR (Inst, InstKind (InstStateTy), InstLabel)
import Registers (RegType (RFloat, RInt), Register (Register))
import Syntax (RelationBinOp (..))

-- | The last instruction of a block
data Terminator ty where
    TJmp :: InstLabel -> Terminator ty
    TBranch ::
        RelationBinOp ->
        Register a ->
        Register a ->
        InstLabel ->
        InstLabel ->
        Terminator ty
    TReturn :: Terminator ty

instance DisplayI (Terminator ty) where
    displayI _ (TJmp l) = "jmp " <> l
    displayI depth (TBranch op r1 r2 l1 l2) =
        "b"
            <> floatPrefix
            <> opPrefix
            <> " "
            <> display r1
            <> ", "
            <> display r2
            <> ", "
            <> l1
            <> "\n"
            <> insertIndent depth
            <> "jmp "
            <> l2
      where
        opPrefix = case op of
            Eq -> "eq"
            Ne -> "ne"
            Lt -> "lt"
            Ge -> "ge"
        floatPrefix = case r1 of
            Register RFloat _ -> "f"
            Register RInt _ -> ""
    displayI _ TReturn = "ret"

-- | A block that holds instructions and a terminator
data CodeBlock ty = CodeBlock
    { blockName :: InstLabel
    , blockInst :: [Inst ty]
    , prevBlocks :: [InstLabel]
    , terminator :: Terminator ty
    }

instance (Display (InstStateTy ty)) => Display (CodeBlock ty) where
    display (CodeBlock l inst _ term) =
        l
            <> ":"
            <> intercalate "" (map (\i -> "\n" <> insertIndent 1 <> displayI 1 i) inst)
            <> "\n"
            <> insertIndent 1
            <> displayI 1 term

-- | Get the next blocks from a block
nextBlocks :: CodeBlock ty -> [InstLabel]
nextBlocks (CodeBlock _ _ _ TReturn) = []
nextBlocks (CodeBlock _ _ _ (TJmp l)) = [l]
nextBlocks (CodeBlock _ _ _ (TBranch _ _ _ l1 l2)) = [l1, l2]

data BlockGraph ty = BlockGraph
    { blocks :: [CodeBlock ty]
    , entryBlock :: InstLabel
    }

instance (Display (InstStateTy ty)) => Display (BlockGraph ty) where
    display (BlockGraph b _) = intercalate "\n" $ map display b

lookupBlock :: InstLabel -> BlockGraph ty -> Maybe (CodeBlock ty)
lookupBlock l block = lookup l $ map (\b -> (blockName b, b)) $ blocks block

asMermaidGraph :: BlockGraph ty -> Text
asMermaidGraph graph =
    "# "
        <> entryBlock graph
        <> "\n```mermaid\ngraph TD\n"
        <> intercalate "\n" (map blockToNode $ blocks graph)
        <> "\n"
        <> "```"
  where
    blockToNode block = intercalate "\n" $ map (\n -> blockName block <> "-->" <> n) $ nextBlocks block
