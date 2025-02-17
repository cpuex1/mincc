{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module CodeBlock (
    CodeBlock (..),
    BlockGraph (..),
    Terminator (..),
    nextBlocks,
    visitInst,
    lookupBlock,
    mapRegBlock,
    mapRegGraph,
    asMermaidGraph,
    visitBlock,
    VirtualBlock,
    VirtualBlockGraph,
    PhiFreeBlock,
    PhiFreeGraph,
) where

import Data.List (find)
import Data.Text (Text, intercalate)
import Display (Display (display), DisplayI (displayI), insertIndent)
import IR (AbstInstKind, Inst, InstKind (InstStateTy), InstLabel, PhiFreeInstKind, mapReg)
import Registers (RegType (RFloat, RInt), Register (Register))
import Syntax (RelationBinOp (..))

-- | The last instruction of a block
data Terminator where
    TJmp :: InstLabel -> Terminator
    TBranch ::
        RelationBinOp ->
        Register a ->
        Register a ->
        InstLabel ->
        InstLabel ->
        Terminator
    TReturn :: Terminator

deriving instance Show Terminator

instance Eq Terminator where
    TJmp l1 == TJmp l2 = l1 == l2
    TBranch op1 r1@(Register RInt _) r2 l1 l2 == TBranch op2 r3@(Register RInt _) r4 l3 l4 =
        op1 == op2 && r1 == r3 && r2 == r4 && l1 == l3 && l2 == l4
    TBranch op1 r1@(Register RFloat _) r2 l1 l2 == TBranch op2 r3@(Register RFloat _) r4 l3 l4 =
        op1 == op2 && r1 == r3 && r2 == r4 && l1 == l3 && l2 == l4
    TReturn == TReturn = True
    _ == _ = False

instance DisplayI Terminator where
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
    , terminator :: Terminator
    }

deriving instance (Show (InstStateTy ty)) => Show (CodeBlock ty)
deriving instance (Eq (InstStateTy ty)) => Eq (CodeBlock ty)

instance (Display (InstStateTy ty)) => Display (CodeBlock ty) where
    display (CodeBlock l inst _ term) =
        l
            <> ":"
            <> intercalate "" (map (\i -> "\n" <> insertIndent 1 <> displayI 1 i) inst)
            <> "\n"
            <> insertIndent 1
            <> displayI 1 term

-- | Get the next blocks from a block.
nextBlocks :: CodeBlock ty -> [InstLabel]
nextBlocks (CodeBlock _ _ _ TReturn) = []
nextBlocks (CodeBlock _ _ _ (TJmp l)) = [l]
nextBlocks (CodeBlock _ _ _ (TBranch _ _ _ l1 l2)) = [l1, l2]

-- | Visit each instruction in a block.
visitInst :: (Monad m) => (Inst ty1 -> m (Inst ty2)) -> CodeBlock ty1 -> m (CodeBlock ty2)
visitInst f (CodeBlock l inst prev term) = do
    inst' <- mapM f inst
    return $ CodeBlock l inst' prev term

-- | Map registers in a block.
mapRegBlock ::
    (forall regTy1. Register regTy1 -> Register regTy1) ->
    CodeBlock ty ->
    CodeBlock ty
mapRegBlock f (CodeBlock l inst prev term) =
    CodeBlock l (map (mapReg f) inst) prev (mapRegTerm term)
  where
    mapRegTerm :: Terminator -> Terminator
    mapRegTerm (TBranch op r1 r2 l1 l2) = TBranch op (f r1) (f r2) l1 l2
    mapRegTerm t = t

data BlockGraph ty = BlockGraph
    { graphBlocks :: [CodeBlock ty]
    , entryBlock :: InstLabel
    , localVars :: Int
    }

deriving instance (Show (InstStateTy ty)) => Show (BlockGraph ty)
deriving instance (Eq (InstStateTy ty)) => Eq (BlockGraph ty)

instance (Display (InstStateTy ty)) => Display (BlockGraph ty) where
    display graph = intercalate "\n" $ map display $ graphBlocks graph

-- | Lookup a block by its label.
lookupBlock :: InstLabel -> [CodeBlock ty] -> Maybe (CodeBlock ty)
lookupBlock l = find (\b -> blockName b == l)

-- | Visit each block in a graph.
visitBlock :: (Monad m) => (CodeBlock ty1 -> m (CodeBlock ty2)) -> BlockGraph ty1 -> m (BlockGraph ty2)
visitBlock f (BlockGraph b e l) = do
    b' <- mapM f b
    return $ BlockGraph b' e l

-- | Map registers in a block graph.
mapRegGraph :: (forall regTy1. Register regTy1 -> Register regTy1) -> BlockGraph ty -> BlockGraph ty
mapRegGraph f (BlockGraph b e l) = BlockGraph (map (mapRegBlock f) b) e l

-- | Export a block graph as a Mermaid-style graph.
asMermaidGraph :: BlockGraph ty -> Text
asMermaidGraph graph =
    "# "
        <> entryBlock graph
        <> "\n```mermaid\ngraph TD\n"
        <> intercalate "\n" (map blockToNode $ graphBlocks graph)
        <> "\n"
        <> "```"
  where
    blockToNode block = intercalate "\n" $ map (\n -> blockName block <> "-->" <> n) $ nextBlocks block

type VirtualBlock = CodeBlock AbstInstKind
type VirtualBlockGraph = BlockGraph AbstInstKind

type PhiFreeBlock = CodeBlock PhiFreeInstKind
type PhiFreeGraph = BlockGraph PhiFreeInstKind
