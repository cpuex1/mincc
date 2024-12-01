{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.Transform (transformCodeBlock, CodeBlockGenState, insertBuf) where

import Backend.Asm
import Backend.Shuffle (shuffleRegs)
import Control.Monad.State (State, execState, gets, modify)
import Data.Text (Text, pack)
import Syntax (IntBinOp (Add), Loc, dummyLoc)
import Prelude hiding (lookup)

data CodeBlockGenEnv stateTy idTy = CodeBlockGenEnv
    { blocks :: [CodeBlock stateTy idTy]
    , mainLabel :: InstLabel
    , currentLabel :: InstLabel
    , generatedLabel :: Int
    , instBuf :: [Inst stateTy idTy DisallowBranch]
    , currentTerm :: InstTerm stateTy idTy
    , getLocalVars :: Int
    }
    deriving (Show, Eq)

type CodeBlockGenState stateTy idTy a = State (CodeBlockGenEnv stateTy idTy) a

insertBuf :: Inst stateTy idTy DisallowBranch -> CodeBlockGenState stateTy idTy ()
insertBuf i =
    modify $ \e ->
        e
            { instBuf = instBuf e ++ [i]
            }

epilogue :: CodeBlockGenState Loc RegID [Inst Loc RegID DisallowBranch]
epilogue = do
    localVars' <- gets getLocalVars
    pure
        [ ILoad dummyLoc ReturnReg StackReg (4 * localVars')
        , IIntOp dummyLoc Add StackReg StackReg (Imm (4 * (localVars' + 1)))
        ]

flushBuf :: CodeBlockGenState Loc RegID ()
flushBuf = do
    ep <- epilogue
    modify $ \e ->
        e
            { blocks =
                blocks e
                    ++ [ CodeBlock
                            (currentLabel e)
                            (instBuf e ++ if currentTerm e == Return then ep else [])
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

tailCallLabel :: InstLabel -> InstLabel
tailCallLabel label = label <> "_start"

tailRecCallLabel :: InstLabel -> InstLabel
tailRecCallLabel label = label <> "_rec"

insertIShuffle :: Loc -> [(Register RegID Int, Register RegID Int)] -> CodeBlockGenState Loc RegID ()
insertIShuffle state assign =
    mapM_ (\(r1, r2) -> insertBuf $ IMov state r1 (Reg r2)) $ shuffleRegs assign

insertFShuffle :: Loc -> [(Register RegID Float, Register RegID Float)] -> CodeBlockGenState Loc RegID ()
insertFShuffle state assign =
    mapM_ (\(r1, r2) -> insertBuf $ IFMov state r1 (Reg r2)) $ shuffleRegs assign

transformCodeBlock :: IntermediateCodeBlock Loc RegID -> [CodeBlock Loc RegID]
transformCodeBlock (IntermediateCodeBlock label localVars' inst) =
    if label == "__entry"
        then
            -- If the block is the entry block, we can skip the prologue and the epilogue.
            -- Also, we need to add a jump to the exit block.
            blocks $
                execState (traverseInst inst) $
                    CodeBlockGenEnv [] label label 0 [] (Jmp "__exit") localVars'
        else
            blocks
                $ execState
                    ( do
                        insertPrologue
                        traverseInst inst
                    )
                $ CodeBlockGenEnv [] label label 0 [] Return localVars'
  where
    insertPrologue :: CodeBlockGenState Loc RegID ()
    insertPrologue = do
        term <- gets currentTerm
        modify $ \env ->
            env
                { instBuf =
                    [ IIntOp dummyLoc Add StackReg StackReg (Imm (-4))
                    , IStore dummyLoc ReturnReg StackReg 0
                    ]
                , currentTerm = Nop
                }
        flushBuf
        modify $ \env ->
            env
                { currentLabel = tailCallLabel $ mainLabel env
                , instBuf =
                    [ IIntOp dummyLoc Add StackReg StackReg (Imm (-(4 * localVars')))
                    ]
                , currentTerm = Nop
                }
        flushBuf
        modify $ \env ->
            env
                { currentLabel = tailRecCallLabel $ mainLabel env
                , currentTerm = term
                }

    traverseInst :: [Inst Loc RegID AllowBranch] -> CodeBlockGenState Loc RegID ()
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
        if term == Return
            then do
                if mainLabel' == label'
                    then do
                        -- Found a tail rec call!
                        -- Shuffle arguments.
                        insertIShuffle state $ zipWith (\i a -> (ArgsReg i, a)) [0 ..] iArgs
                        insertFShuffle state $ zipWith (\i a -> (ArgsReg i, a)) [0 ..] fArgs
                        -- Jump to the rec label of the function instead.
                        -- The prologue should be skipped.
                        modify $ \env ->
                            env
                                { currentTerm = Jmp $ tailRecCallLabel $ mainLabel env
                                }
                        flushBuf
                    else do
                        -- Found a tail call!
                        -- Shuffle arguments.
                        insertIShuffle state $ zipWith (\i a -> (ArgsReg i, a)) [0 ..] iArgs
                        insertFShuffle state $ zipWith (\i a -> (ArgsReg i, a)) [0 ..] fArgs
                        -- Jump to the start label of the function instead.
                        -- The prologue should be skipped.
                        insertBuf $ IIntOp state Add StackReg StackReg (Imm (4 * localVars'))
                        modify $ \env ->
                            env
                                { currentTerm = Jmp $ tailCallLabel $ mainLabel env
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

    transformInst :: Inst Loc RegID AllowBranch -> CodeBlockGenState Loc RegID ()
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
        -- Shuffle arguments
        insertIShuffle state $ zipWith (\i a -> (ArgsReg i, a)) [0 ..] iArgs
        insertFShuffle state $ zipWith (\i a -> (ArgsReg i, a)) [0 ..] fArgs
        insertBuf $ ICall state label'
    transformInst (IClosureCall state cl iArgs fArgs) = do
        -- Make sure the closure is not in an argument register.
        cl' <- case cl of
            ArgsReg _ -> do
                insertBuf $ IMov state (TempReg 1) (Reg cl)
                pure $ TempReg 1
            _ -> pure cl

        -- Shuffle arguments
        insertIShuffle state $ zipWith (\i a -> (ArgsReg i, a)) [0 ..] iArgs
        insertFShuffle state $ zipWith (\i a -> (ArgsReg i, a)) [0 ..] fArgs
        insertBuf $ IIntOp state Add (ArgsReg (length iArgs)) cl' (Imm 4)
        insertBuf $ ILoad state (TempReg 2) cl' 0
        insertBuf $ ICallReg state (TempReg 2)
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
    transformInst (IRawInst state name ret iArgs fArgs) =
        insertBuf $ IRawInst state name ret iArgs fArgs
    transformInst (IBranch{}) = undefined
