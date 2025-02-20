{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module BackEnd.Transform (transformCodeBlock, CodeBlockGenStateT) where

import BackEnd.BackendEnv (BackendEnv (globals), BackendStateT)
import BackEnd.Shuffle (shuffleRegOrImm, shuffleRegs)
import Control.Monad.State (MonadTrans (lift), StateT, execStateT, gets, modify)
import Data.Text (Text, isPrefixOf, pack)
import IR (
    AbstCodeBlock,
    AbstInst,
    HCodeBlock (HCodeBlock),
    Inst (..),
    InstLabel,
    InstTerm (..),
    LCodeBlock (LCodeBlock),
    PrimitiveIntOp (PAdd),
    RawCodeBlock,
    RawInst,
    RawInstTerm,
    isTerm,
 )
import MiddleEnd.Globals (
    GlobalTable (endAddr),
 )
import Registers (
    RegID,
    RegOrImm (Imm, Reg),
    RegType (RFloat, RInt),
    Register (Register),
    RegisterKind (ArgsReg, ZeroReg),
    argsReg,
    heapReg,
    returnReg,
    stackReg,
    tempReg,
 )
import Syntax (Loc, dummyLoc)
import Prelude hiding (lookup)

data CodeBlockGenEnv = CodeBlockGenEnv
    { blocks :: [RawCodeBlock]
    , mainLabel :: InstLabel
    , currentLabel :: InstLabel
    , generatedLabel :: Int
    , instBuf :: [RawInst]
    , currentTerm :: RawInstTerm
    , getLocalVars :: Int
    }

type CodeBlockGenStateT m = StateT CodeBlockGenEnv (BackendStateT m)

insertBuf :: (Monad m) => RawInst -> CodeBlockGenStateT m ()
insertBuf i =
    modify $ \e ->
        e
            { instBuf = instBuf e ++ [i]
            }

epilogue :: (Monad m) => CodeBlockGenStateT m [RawInst]
epilogue = do
    localVars' <- gets getLocalVars
    pure
        [ ILoad dummyLoc returnReg stackReg localVars'
        , IIntOp dummyLoc PAdd stackReg stackReg (Imm RInt (localVars' + 1))
        ]

flushBuf :: (Monad m) => CodeBlockGenStateT m ()
flushBuf = do
    ep <- epilogue
    modify $ \e ->
        e
            { blocks =
                blocks e
                    ++ [ LCodeBlock
                            (currentLabel e)
                            ( instBuf e ++ case currentTerm e of
                                Return -> ep
                                _ -> []
                            )
                            (currentTerm e)
                       ]
            , instBuf = []
            }

genLabel :: (Monad m) => Text -> CodeBlockGenStateT m InstLabel
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

insertIShuffle :: (Monad m) => Loc -> [(Register Int, RegOrImm Int)] -> CodeBlockGenStateT m ()
insertIShuffle state assign =
    mapM_ (\(r1, r2) -> insertBuf $ IMov state r1 r2) $ shuffleRegOrImm RInt assign

insertFShuffle :: (Monad m) => Loc -> [(Register Float, Register Float)] -> CodeBlockGenStateT m ()
insertFShuffle state assign =
    mapM_ (\(r1, r2) -> insertBuf $ IMov state r1 (Reg r2)) $ shuffleRegs RFloat assign

initializeGlobal :: (Monad m) => CodeBlockGenStateT m ()
initializeGlobal = do
    global <- lift (gets globals)
    insertBuf $ IMov dummyLoc heapReg (Imm RInt $ endAddr global)

transformCodeBlock :: (Monad m) => AbstCodeBlock -> BackendStateT m [RawCodeBlock]
transformCodeBlock (HCodeBlock label localVars' inst) =
    if label == "__entry"
        then do
            -- If the block is the entry block, we can skip the prologue and the epilogue.
            -- Also, we need to add a jump to the exit block.
            blocks
                <$> execStateT
                    (initializeGlobal >> traverseInst inst)
                    ( CodeBlockGenEnv [] label label 0 [] (Jmp "__exit") localVars'
                    )
        else do
            blocks
                <$> execStateT
                    ( do
                        insertPrologue
                        traverseInst inst
                    )
                    (CodeBlockGenEnv [] label label 0 [] Return localVars')
  where
    insertPrologue :: (Monad m) => CodeBlockGenStateT m ()
    insertPrologue = do
        term <- gets currentTerm
        modify $ \env ->
            env
                { instBuf =
                    [ IIntOp dummyLoc PAdd stackReg stackReg (Imm RInt (-1))
                    , IStore dummyLoc returnReg stackReg 0
                    ]
                , currentTerm = Nop
                }
        flushBuf
        modify $ \env ->
            env
                { currentLabel = tailCallLabel $ mainLabel env
                , instBuf =
                    [IIntOp dummyLoc PAdd stackReg stackReg (Imm RInt (-localVars')) | localVars' /= 0]
                , currentTerm = Nop
                }
        flushBuf
        modify $ \env ->
            env
                { currentLabel = tailRecCallLabel $ mainLabel env
                , currentTerm = term
                }

    traverseInst :: (Monad m) => [AbstInst] -> CodeBlockGenStateT m ()
    traverseInst [] = flushBuf
    -- traverseInst [IRichCall state label' iArgs fArgs] = do
    --     -- Check whether tail recursion optimization can be applied.
    --     mainLabel' <- gets mainLabel
    --     term <- gets currentTerm
    --     if "__ext_" `isPrefixOf` label'
    --         then do
    --             -- External functions should be called directly.
    --             transformInst $ IRichCall state label' iArgs fArgs
    --             flushBuf
    --         else case term of
    --             Return -> do
    --                 if mainLabel' == label'
    --                     then do
    --                         -- Found a tail rec call!
    --                         -- Shuffle arguments.
    --                         insertIShuffle state $ zipWith (\i a -> (argsReg RInt i, a)) [0 ..] iArgs
    --                         insertFShuffle state $ zipWith (\i a -> (argsReg RFloat i, a)) [0 ..] fArgs
    --                         -- Jump to the rec label of the function instead.
    --                         -- The prologue should be skipped.
    --                         modify $ \env ->
    --                             env
    --                                 { currentTerm = Jmp $ tailRecCallLabel $ mainLabel env
    --                                 }
    --                         flushBuf
    --                     else do
    --                         -- Found a tail call!
    --                         -- Shuffle arguments.
    --                         insertIShuffle state $ zipWith (\i a -> (argsReg RInt i, a)) [0 ..] iArgs
    --                         insertFShuffle state $ zipWith (\i a -> (argsReg RFloat i, a)) [0 ..] fArgs
    --                         -- Jump to the start label of the function instead.
    --                         -- The prologue should be skipped.
    --                         insertBuf $ IIntOp state PAdd stackReg stackReg (Imm RInt localVars')
    --                         modify $ \env ->
    --                             env
    --                                 { currentTerm = Jmp $ tailCallLabel label'
    --                                 }
    --                         flushBuf
    --             _ -> do
    --                 -- If not, just call a function.
    --                 transformInst $ IRichCall state label' iArgs fArgs
    --                 flushBuf
    traverseInst [inst'] = do
        transformInst inst'
        flushBuf
    traverseInst (inst' : rest) = do
        transformInst inst'
        traverseInst rest

    transformInst :: (Monad m) => AbstInst -> CodeBlockGenStateT m ()
    transformInst (ICompOp state op dest src1 src2) =
        insertBuf $ ICompOp state op dest src1 src2
    transformInst (IIntOp state op dest src1 src2) =
        insertBuf $ IIntOp state op dest src1 src2
    transformInst (IFOp state op dest src1 src2) =
        insertBuf $ IFOp state op dest src1 src2
    transformInst (IMov _ (Register _ ZeroReg) _) = pure ()
    transformInst (IMov state dest src) =
        insertBuf $ IMov state dest src
    transformInst (ILoad _ (Register RInt ZeroReg) _ _) =
        pure ()
    transformInst (ILoad state dest src offset) =
        insertBuf $ ILoad state dest src offset
    transformInst (IStore state dest src offset) =
        insertBuf $ IStore state dest src offset
    transformInst (IRawInst state name ret iArgs fArgs) =
        insertBuf $ IRawInst state name ret iArgs fArgs
