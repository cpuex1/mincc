{-# LANGUAGE OverloadedStrings #-}

module RegisterAlloc (runRegAlloc) where

import BackEnd.Analysis.Liveness (runGraphLiveness)
import BackEnd.BackendEnv (liftB)
import BackEnd.RegisterAlloc (RegAllocResult (numReg), assignReg, spillTarget)
import BackEnd.Spill (spill)
import CodeBlock (BlockGraph (entryBlock), PhiFreeGraph, VirtualBlockGraph)
import CommandLine (BackendIdentStateIO, CompilerConfig (cRegLimit))
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Reader (asks)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Text (pack)
import Display (Display (display))
import Error (CompilerError (OtherError))
import Log (LogLevel (Debug), printTextLog)
import Registers (RegType (RFloat, RInt), savedReg, (#!!))

-- | Run register allocation.
runRegAlloc :: VirtualBlockGraph -> BackendIdentStateIO PhiFreeGraph
runRegAlloc graph = do
    liftB $ lift $ printTextLog Debug $ "Begin register allocation for " <> entryBlock graph

    -- Do register allocation.
    let (result, phiFreeGraph) = assignReg $ runGraphLiveness graph

    -- Spill the register if necessary.
    iSpilt <- runSpill RInt (result #!! RInt) graph
    case iSpilt of
        Just iSplit' -> runRegAlloc iSplit'
        Nothing -> do
            fSpilt <- runSpill RFloat (result #!! RFloat) graph
            case fSpilt of
                Just fSplit' -> runRegAlloc fSplit'
                Nothing ->
                    -- There is no register to spill.
                    pure phiFreeGraph

-- | Run register spilling.
runSpill :: RegType a -> RegAllocResult -> VirtualBlockGraph -> BackendIdentStateIO (Maybe VirtualBlockGraph)
runSpill rTy result graph = do
    limit <- liftB $ asks ((#!! rTy) . cRegLimit)
    liftB $ lift $ printTextLog Debug $ "The number of " <> display rTy <> " registers required: " <> pack (show (numReg result))
    if limit < numReg result
        then do
            case spillTarget result of
                Just reg -> do
                    -- Spill the register.
                    graph' <- spill rTy reg graph
                    liftB $ lift $ printTextLog Debug $ "Bound " <> display (savedReg rTy reg) <> " to a local variable."
                    pure $ Just graph'
                Nothing ->
                    throwError $
                        OtherError $
                            "Encountered a shortage of "
                                <> display rTy
                                <> " registers. You may need to increase the register limit."
        else do
            pure Nothing
