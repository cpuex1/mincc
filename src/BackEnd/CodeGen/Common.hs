{-# LANGUAGE OverloadedStrings #-}

module BackEnd.CodeGen.Common (
    CodeGen (..),
    codeGen,
    exitGraph,
) where

import BackEnd.BackendEnv (BackendEnv (globals), BackendStateT)
import CodeBlock (
    BlockGraph (..),
    CodeBlock (..),
    PhiFreeBlock,
    PhiFreeGraph,
    Terminator (..),
    visitBlock,
 )
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State (gets)
import Data.List (unsnoc)
import Data.Proxy (Proxy)
import Data.Text (Text, isPrefixOf)
import IR (Inst (ICall, IIntOp, ILoad, IMov, IStore), InstLabel, PrimitiveIntOp (PAdd))
import MiddleEnd.Globals (GlobalTable (endAddr))
import Registers (RegOrImm (Imm), RegType (RInt), heapReg, returnReg, stackReg)
import Syntax (dummyLoc)

-- | A very simple class for code generation.
class CodeGen a where
    -- | DO NOT CALL THIS FUNCTION DIRECTLY.
    -- Call `generateCode` instead.
    codeGenInternal :: Proxy a -> PhiFreeGraph -> Text

-- | Generates a code.
codeGen :: (Monad m, CodeGen a) => Proxy a -> PhiFreeGraph -> BackendStateT m Text
codeGen proxy graph = do
    initialized <- initGlobalTable graph
    let inserted = insertPrologueBlocks initialized
    let expanded = runIdentity (visitBlock (pure . expandReturn (entryBlock graph) (localVars graph)) inserted)
    pure $ codeGenInternal proxy expanded

-- | Initialize the global table.
initGlobalTable :: (Monad m) => PhiFreeGraph -> BackendStateT m PhiFreeGraph
initGlobalTable graph@(BlockGraph blocks "__entry" _) = do
    blocks' <-
        mapM
            ( \block -> do
                case blockName block of
                    "__entry" -> do
                        addr <- gets (endAddr . globals)
                        pure $ block{blockInst = IMov dummyLoc heapReg (Imm RInt addr) : blockInst block}
                    _ -> pure block
            )
            blocks
    pure graph{graphBlocks = blocks'}
initGlobalTable graph = pure graph

-- | Inserts the prologue blocks.
insertPrologueBlocks :: PhiFreeGraph -> PhiFreeGraph
insertPrologueBlocks graph@(BlockGraph _ "__entry" _) = graph
insertPrologueBlocks graph@(BlockGraph _ "__exit" _) = graph
insertPrologueBlocks graph =
    renamedGraph{entryBlock = entryLabel, graphBlocks = [firstBlock, startBlock] <> graphBlocks renamedGraph}
  where
    -- Create the labels.
    entryLabel = entryBlock graph
    startLabel = entryLabel <> "_start"
    recLabel = entryLabel <> "_rec"

    -- Rename the entry block.
    renamedGraph =
        runIdentity
            ( visitBlock
                ( \block ->
                    if blockName block == entryLabel
                        then
                            pure block{blockName = recLabel, prevBlocks = [startLabel]}
                        else
                            pure block
                )
                graph
            )

    -- Store the ra register to the stack.
    firstBlock =
        CodeBlock
            entryLabel
            [ IIntOp dummyLoc PAdd stackReg stackReg (Imm RInt (-1))
            , IStore dummyLoc returnReg stackReg 0
            ]
            []
            (TJmp startLabel)

    -- Allocate memory space for local variables.
    startBlock =
        CodeBlock
            startLabel
            [ IIntOp dummyLoc PAdd stackReg stackReg (Imm RInt (-(localVars graph))) | localVars graph > 0
            ]
            [entryLabel]
            (TJmp recLabel)

-- | Expands a return terminator.
expandReturn :: InstLabel -> Int -> PhiFreeBlock -> PhiFreeBlock
expandReturn "__entry" _ block@(CodeBlock _ _ _ TReturn) =
    -- Go to the exit block.
    block{terminator = TJmp "__exit"}
expandReturn entryLabel local block@(CodeBlock _ _ _ TReturn) =
    case unsnoc (blockInst block) of
        Just (inst, ICall _ func) ->
            if func `isPrefixOf` "__ext_"
                then
                    -- The external function cannot be tail-recursive.
                    block
                else
                    if func == entryLabel
                        then
                            -- Remove a tail-recursive call.
                            block{blockInst = inst, terminator = TJmp $ func <> "_rec"}
                        else
                            -- Remove a tail call.
                            block
                                { blockInst = inst ++ [IIntOp dummyLoc PAdd stackReg stackReg (Imm RInt local) | local > 0]
                                , terminator = TJmp $ func <> "_start"
                                }
        _ ->
            -- Restore the ra register and the stack pointer.
            block
                { blockInst =
                    blockInst block
                        ++ [ ILoad dummyLoc returnReg stackReg local
                           , IIntOp dummyLoc PAdd stackReg stackReg (Imm RInt (local + 1))
                           ]
                }
expandReturn _ _ block = block

-- | A finial block.
exitGraph :: PhiFreeGraph
exitGraph =
    BlockGraph [CodeBlock "__exit" [] [] (TJmp "__fin")] "__exit" 0
