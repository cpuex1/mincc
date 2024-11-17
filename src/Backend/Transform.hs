{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.Transform (transformCodeBlock) where

import Backend.Asm
import Control.Monad.State (State, execState, gets, modify)
import Data.Text (Text, pack)
import Syntax (IntBinOp (Add))

data CodeBlockGenEnv stateTy idTy = CodeBlockGenEnv
    { blocks :: [CodeBlock stateTy idTy]
    , mainLabel :: InstLabel
    , currentLabel :: InstLabel
    , generatedLabel :: Int
    , instBuf :: [Inst stateTy idTy DisallowBranch]
    , currentTerm :: InstTerm stateTy idTy
    , epilogue :: [Inst stateTy idTy DisallowBranch]
    }
    deriving (Show, Eq)

type CodeBlockGenState stateTy idTy a = State (CodeBlockGenEnv stateTy idTy) a

insertBuf :: Inst stateTy idTy DisallowBranch -> CodeBlockGenState stateTy idTy ()
insertBuf i =
    modify $ \e ->
        e
            { instBuf = instBuf e ++ [i]
            }

flushBuf :: (Eq stateTy, Eq idTy) => CodeBlockGenState stateTy idTy ()
flushBuf =
    modify $ \e ->
        e
            { blocks =
                blocks e
                    ++ [ CodeBlock
                            (currentLabel e)
                            (instBuf e ++ if currentTerm e == Return then epilogue e else [])
                            (currentTerm e)
                       ]
            , instBuf = []
            }

genLabel :: Text -> CodeBlockGenState stateTy idTy InstLabel
genLabel tag = do
    label <- gets generatedLabel
    mainLabel' <- gets mainLabel
    modify $ \e ->
        e
            { generatedLabel = label + 1
            }
    return $ mainLabel' <> "_" <> tag <> "_" <> pack (show label)

transformCodeBlock :: (Eq stateTy) => IntermediateCodeBlock stateTy RegID -> [CodeBlock stateTy RegID]
transformCodeBlock (IntermediateCodeBlock label prologue inst epilogue') =
    if label == "__entry"
        then
            -- If the block is the entry block, we can skip the prologue and the epilogue.
            -- Also, we need to add a jump to the exit block.
            blocks $
                execState (traverseInst inst) $
                    CodeBlockGenEnv [] label label 0 [] (Jmp "__exit") []
        else
            blocks
                $ execState
                    ( do
                        insertPrologue prologue
                        traverseInst inst
                    )
                $ CodeBlockGenEnv [] label label 0 [] Return epilogue'
  where
    insertPrologue :: (Eq stateTy) => [Inst stateTy RegID DisallowBranch] -> CodeBlockGenState stateTy RegID ()
    insertPrologue prologue' = do
        term <- gets currentTerm
        modify $ \env ->
            env
                { instBuf = prologue'
                , currentTerm = Nop
                }
        flushBuf
        modify $ \env ->
            env
                { currentLabel = mainLabel env <> "_start"
                , currentTerm = term
                }

    traverseInst :: (Eq stateTy) => [Inst stateTy RegID AllowBranch] -> CodeBlockGenState stateTy RegID ()
    traverseInst [] = flushBuf
    traverseInst [IBranch state op left right thenInst elseInst] = do
        term <- gets currentTerm
        elseLabel <- genLabel "else"
        thenLabel <- genLabel "then"
        modify $ \e ->
            e
                { currentTerm = Branch state op left right thenLabel
                }
        flushBuf
        modify $ \e ->
            e
                { currentLabel = elseLabel
                , currentTerm = term
                }
        traverseInst elseInst
        modify $ \e ->
            e
                { currentLabel = thenLabel
                , currentTerm = term
                }
        traverseInst thenInst
    traverseInst (IBranch state op left right thenInst elseInst : rest) = do
        term <- gets currentTerm
        elseLabel <- genLabel "else"
        thenLabel <- genLabel "then"
        endLabel <- genLabel "end"
        modify $ \e ->
            e
                { currentTerm = Branch state op left right thenLabel
                }
        flushBuf
        modify $ \e ->
            e
                { currentLabel = elseLabel
                , currentTerm = Jmp endLabel
                }
        traverseInst elseInst
        modify $ \e ->
            e
                { currentLabel = thenLabel
                , currentTerm = Nop
                }
        traverseInst thenInst
        modify $ \e ->
            e
                { currentLabel = endLabel
                , currentTerm = term
                }
        traverseInst rest
    traverseInst [IRichCall state label' iArgs fArgs] = do
        -- Check whether tail recursion optimization can be applied.
        mainLabel' <- gets mainLabel
        term <- gets currentTerm
        if mainLabel' == label' && term == Return
            then do
                -- Found a tail call!
                -- TODO: shuffle
                mapM_
                    ( \(arg, i) ->
                        insertBuf $ IMov state (ArgsReg i) (Reg arg)
                    )
                    $ zip iArgs [0 ..]
                mapM_
                    ( \(arg, i) ->
                        insertBuf $ IFMov state (ArgsReg i) (Reg arg)
                    )
                    $ zip fArgs [0 ..]
                -- Jump to the start label of the function instead.
                -- The prologue should be skipped.
                modify $ \env ->
                    env
                        { currentTerm = Jmp $ mainLabel env <> "_start"
                        }
                flushBuf
            else do
                -- If not, just call a function.
                transformInst $ IRichCall state label' iArgs fArgs
                flushBuf
    traverseInst [inst'] = do
        transformInst inst'
        flushBuf
    traverseInst (inst' : rest) = do
        transformInst inst'
        traverseInst rest

    transformInst :: Inst stateTy RegID AllowBranch -> CodeBlockGenState stateTy RegID ()
    transformInst (ICompOp state op dest src1 src2) =
        insertBuf $ ICompOp state op dest src1 src2
    transformInst (IFCompOp state op dest src1 src2) =
        insertBuf $ IFCompOp state op dest src1 src2
    transformInst (IIntOp state op dest src1 src2) =
        insertBuf $ IIntOp state op dest src1 src2
    transformInst (IFOp state op dest src1 src2) =
        insertBuf $ IFOp state op dest src1 src2
    transformInst (IMov state dest src) =
        insertBuf $ IMov state dest src
    transformInst (IFMov state dest src) =
        insertBuf $ IFMov state dest src
    transformInst (IRichCall state label' iArgs fArgs) = do
        -- TODO: shuffle
        mapM_
            ( \(arg, i) ->
                insertBuf $ IMov state (ArgsReg i) (Reg arg)
            )
            $ zip iArgs [0 ..]
        mapM_
            ( \(arg, i) ->
                insertBuf $ IFMov state (ArgsReg i) (Reg arg)
            )
            $ zip fArgs [0 ..]
        insertBuf $ ICall state label'
    transformInst (IClosureCall state cl iArgs fArgs) = do
        -- TODO: shuffle
        mapM_
            ( \(arg, i) ->
                insertBuf $ IMov state (ArgsReg i) (Reg arg)
            )
            $ zip iArgs [0 ..]
        insertBuf $ IIntOp state Add (ArgsReg (length iArgs)) cl (Imm 4)
        mapM_
            ( \(arg, i) ->
                insertBuf $ IFMov state (ArgsReg i) (Reg arg)
            )
            $ zip fArgs [0 ..]
        insertBuf $ ILoad state (TempReg 0) cl 0
        insertBuf $ ICallReg state (TempReg 0)
    transformInst (IMakeClosure state dest label' iFreeV fFreeV) = do
        insertBuf $ ILMov state dest label'
        insertBuf $ IStore state dest HeapReg 0
        mapM_
            ( \(arg, i) ->
                insertBuf $ IStore state arg HeapReg (i * 4)
            )
            $ zip iFreeV [1 ..]
        mapM_
            ( \(arg, i) ->
                insertBuf $ IFStore state arg HeapReg (i * 4)
            )
            $ zip fFreeV [1 + length iFreeV ..]
        insertBuf $ IMov state dest (Reg HeapReg)
        insertBuf $ IIntOp state Add HeapReg HeapReg (Imm $ 4 * (1 + length iFreeV + length fFreeV))
    transformInst (ILoad state dest src offset) =
        insertBuf $ ILoad state dest src offset
    transformInst (IStore state dest src offset) =
        insertBuf $ IStore state dest src offset
    transformInst (IFLoad state dest src offset) =
        insertBuf $ IFLoad state dest src offset
    transformInst (IFStore state dest src offset) =
        insertBuf $ IFStore state dest src offset
    transformInst (IBranch{}) = undefined
