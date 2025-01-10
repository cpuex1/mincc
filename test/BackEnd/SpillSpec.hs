{-# LANGUAGE OverloadedStrings #-}

module BackEnd.SpillSpec (spec) where

import BackEnd.BackendEnv (
    BackendEnv (regContext),
    RegContext (generatedReg),
    createBackendConfig,
    runBackendStateT,
 )
import BackEnd.Spill
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State (modify)
import Error (CompilerError)
import IR (
    Inst (..),
    IntermediateCodeBlock (IntermediateCodeBlock),
    PrimitiveIntOp (PAdd),
    RegID,
    RegOrImm (Imm, Reg),
    RegType (RFloat, RInt),
    Register (SavedReg, StackReg),
    updateVariant,
 )
import MiddleEnd.Globals (defaultGlobalTable)
import Syntax (FloatBinOp (FAdd), Loc, dummyLoc)
import Test.Hspec

doSpillI :: Int -> Int -> Int -> IntermediateCodeBlock Loc RegID -> Either CompilerError (IntermediateCodeBlock Loc RegID)
doSpillI generatedI generatedF var block =
    runIdentity
        ( runBackendStateT
            ( do
                modify (\env -> env{regContext = updateVariant RInt (\ctx -> ctx{generatedReg = generatedI}) $ regContext env})
                modify (\env -> env{regContext = updateVariant RFloat (\ctx -> ctx{generatedReg = generatedF}) $ regContext env})
                spillI var block
            )
            (createBackendConfig 10 10)
            defaultGlobalTable
        )

doSpillF :: Int -> Int -> Int -> IntermediateCodeBlock Loc RegID -> Either CompilerError (IntermediateCodeBlock Loc RegID)
doSpillF generatedI generatedF var block =
    runIdentity
        ( runBackendStateT
            ( do
                modify (\env -> env{regContext = updateVariant RInt (\ctx -> ctx{generatedReg = generatedI}) $ regContext env})
                modify (\env -> env{regContext = updateVariant RFloat (\ctx -> ctx{generatedReg = generatedF}) $ regContext env})
                spillF var block
            )
            (createBackendConfig 10 10)
            defaultGlobalTable
        )

spec :: Spec
spec = do
    describe "spillI" $ do
        it "test1" $ do
            doSpillI
                3
                0
                0
                ( IntermediateCodeBlock
                    "test"
                    0
                    [ IMov dummyLoc (SavedReg 0) (Imm 0)
                    , IMov dummyLoc (SavedReg 1) (Imm 1)
                    , IMov dummyLoc (SavedReg 2) (Imm 2)
                    , IIntOp dummyLoc PAdd (SavedReg 1) (SavedReg 1) (Reg (SavedReg 2))
                    , IIntOp dummyLoc PAdd (SavedReg 0) (SavedReg 0) (Reg (SavedReg 1))
                    ]
                )
                `shouldBe` Right
                    ( IntermediateCodeBlock
                        "test"
                        1
                        [ IMov dummyLoc (SavedReg 3) (Imm 0)
                        , IStore dummyLoc (SavedReg 3) StackReg 0
                        , IMov dummyLoc (SavedReg 1) (Imm 1)
                        , IMov dummyLoc (SavedReg 2) (Imm 2)
                        , IIntOp dummyLoc PAdd (SavedReg 1) (SavedReg 1) (Reg (SavedReg 2))
                        , ILoad dummyLoc (SavedReg 4) StackReg 0
                        , IIntOp dummyLoc PAdd (SavedReg 5) (SavedReg 4) (Reg (SavedReg 1))
                        , IStore dummyLoc (SavedReg 5) StackReg 0
                        ]
                    )
        it "test2" $ do
            doSpillF
                0
                3
                0
                ( IntermediateCodeBlock
                    "test"
                    0
                    [ IFMov dummyLoc (SavedReg 0) (Imm 0)
                    , IFMov dummyLoc (SavedReg 1) (Imm 1)
                    , IFMov dummyLoc (SavedReg 2) (Imm 2)
                    , IFOp dummyLoc FAdd (SavedReg 1) (SavedReg 1) (SavedReg 2)
                    , IFOp dummyLoc FAdd (SavedReg 0) (SavedReg 0) (SavedReg 1)
                    ]
                )
                `shouldBe` Right
                    ( IntermediateCodeBlock
                        "test"
                        1
                        [ IFMov dummyLoc (SavedReg 3) (Imm 0)
                        , IFStore dummyLoc (SavedReg 3) StackReg 0
                        , IFMov dummyLoc (SavedReg 1) (Imm 1)
                        , IFMov dummyLoc (SavedReg 2) (Imm 2)
                        , IFOp dummyLoc FAdd (SavedReg 1) (SavedReg 1) (SavedReg 2)
                        , IFLoad dummyLoc (SavedReg 4) StackReg 0
                        , IFOp dummyLoc FAdd (SavedReg 5) (SavedReg 4) (SavedReg 1)
                        , IFStore dummyLoc (SavedReg 5) StackReg 0
                        ]
                    )
