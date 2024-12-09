{-# LANGUAGE OverloadedStrings #-}

module Backend.SpillSpec (spec) where

import Backend.Asm (Inst (IFLoad, IFMov, IFOp, IFStore, IIntOp, ILoad, IMov, IStore), IntermediateCodeBlock (IntermediateCodeBlock), RegOrImm (Imm, Reg), Register (SavedReg, StackReg))
import Backend.BackendEnv (BackendConfig (BackendConfig), BackendEnv (generatedFReg, generatedIReg), RegID, runBackendStateT)
import Backend.Spill
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State (modify)
import Error (CompilerError)
import Globals (defaultGlobalTable)
import Syntax (FloatBinOp (FAdd), IntBinOp (Add), Loc, dummyLoc)
import Test.Hspec

doSpillI :: Int -> Int -> Int -> IntermediateCodeBlock Loc RegID -> Either CompilerError (IntermediateCodeBlock Loc RegID)
doSpillI generatedI generatedF var block =
    runIdentity
        ( runBackendStateT
            ( do
                modify (\env -> env{generatedIReg = generatedI, generatedFReg = generatedF})
                spillI var block
            )
            (BackendConfig 10 10)
            defaultGlobalTable
        )

doSpillF :: Int -> Int -> Int -> IntermediateCodeBlock Loc RegID -> Either CompilerError (IntermediateCodeBlock Loc RegID)
doSpillF generatedI generatedF var block =
    runIdentity
        ( runBackendStateT
            ( do
                modify (\env -> env{generatedIReg = generatedI, generatedFReg = generatedF})
                spillF var block
            )
            (BackendConfig 10 10)
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
                    , IIntOp dummyLoc Add (SavedReg 1) (SavedReg 1) (Reg (SavedReg 2))
                    , IIntOp dummyLoc Add (SavedReg 0) (SavedReg 0) (Reg (SavedReg 1))
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
                        , IIntOp dummyLoc Add (SavedReg 1) (SavedReg 1) (Reg (SavedReg 2))
                        , ILoad dummyLoc (SavedReg 4) StackReg 0
                        , IIntOp dummyLoc Add (SavedReg 5) (SavedReg 4) (Reg (SavedReg 1))
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
