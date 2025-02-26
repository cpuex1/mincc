{-# LANGUAGE OverloadedStrings #-}

module BackEnd.CodeGen.RINANA (CodeGenRINANA) where

import BackEnd.CodeGen.Common (CodeGen (..))
import BackEnd.CodeGen.Optim (removeMeaninglessAssignment)
import CodeBlock (
    BlockGraph (..),
    CodeBlock (..),
    PhiFreeBlock,
    Terminator (TBranch, TJmp),
 )
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text, intercalate, lines)
import Display (DisplayI (displayI), insertIndent)
import IR (InstLabel)
import Prelude hiding (lines)

-- | The code generator for RINANA.
data CodeGenRINANA = CodeGenRINANA
    deriving (Show, Eq)

-- | A code generator for the terminator.
codeGenTerminator :: Maybe InstLabel -> Terminator -> Text
codeGenTerminator _ (TJmp "__fin") =
    -- A special label for the end of the program.
    "\n" <> insertIndent 1 <> "hlt"
codeGenTerminator nextLabel term@(TJmp label) =
    if nextLabel == Just label
        then
            ""
        else
            "\n" <> insertIndent 1 <> displayI 1 term
codeGenTerminator nextLabel term@(TBranch _ _ _ _ elseLabel) =
    if nextLabel == Just elseLabel
        then
            -- Removes the last line.
            "\n" <> insertIndent 1 <> fromMaybe "" (listToMaybe $ lines $ displayI 1 term)
        else
            "\n" <> insertIndent 1 <> displayI 1 term
codeGenTerminator _ term = "\n" <> insertIndent 1 <> displayI 1 term

-- | A code generator for a block.
codeGenBlock :: Maybe InstLabel -> PhiFreeBlock -> Text
codeGenBlock nextBlock block =
    blockName block
        <> ":"
        <> intercalate "" (map (\i -> "\n" <> insertIndent 1 <> displayI 1 i) $ blockInst block)
        <> codeGenTerminator nextBlock (terminator block)

-- | Gets the next label.
withNext :: [PhiFreeBlock] -> [Maybe InstLabel]
withNext [] = []
withNext (_ : blocks) =
    map (Just . blockName) blocks ++ [Nothing]

instance CodeGen CodeGenRINANA where
    codeGenInternal _ graph =
        intercalate "\n" generated
      where
        blocks = graphBlocks $ removeMeaninglessAssignment graph
        generated = zipWith codeGenBlock (withNext blocks) blocks
