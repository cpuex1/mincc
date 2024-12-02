{-# LANGUAGE OverloadedStrings #-}

module Backend.SpillSpec (spec) where

import Backend.Asm (Inst (IFLoad, IFMov, IFOp, IFStore, IIntOp, ILoad, IMov, IStore), IntermediateCodeBlock (IntermediateCodeBlock), RegOrImm (Imm, Reg), Register (StackReg, TempReg))
import Backend.BackendEnv (BackendConfig (BackendConfig), BackendEnv (generatedFReg, generatedIReg), RegID, runBackendStateT)
import Backend.Spill
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State (modify)
import Error (CompilerError)
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
                    [ IMov dummyLoc (TempReg 0) (Imm 0)
                    , IMov dummyLoc (TempReg 1) (Imm 1)
                    , IMov dummyLoc (TempReg 2) (Imm 2)
                    , IIntOp dummyLoc Add (TempReg 1) (TempReg 1) (Reg (TempReg 2))
                    , IIntOp dummyLoc Add (TempReg 0) (TempReg 0) (Reg (TempReg 1))
                    ]
                )
                `shouldBe` Right
                    ( IntermediateCodeBlock
                        "test"
                        1
                        [ IMov dummyLoc (TempReg 3) (Imm 0)
                        , IStore dummyLoc (TempReg 3) StackReg 0
                        , IMov dummyLoc (TempReg 1) (Imm 1)
                        , IMov dummyLoc (TempReg 2) (Imm 2)
                        , IIntOp dummyLoc Add (TempReg 1) (TempReg 1) (Reg (TempReg 2))
                        , ILoad dummyLoc (TempReg 4) StackReg 0
                        , IIntOp dummyLoc Add (TempReg 5) (TempReg 4) (Reg (TempReg 1))
                        , IStore dummyLoc (TempReg 5) StackReg 0
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
                    [ IFMov dummyLoc (TempReg 0) (Imm 0)
                    , IFMov dummyLoc (TempReg 1) (Imm 1)
                    , IFMov dummyLoc (TempReg 2) (Imm 2)
                    , IFOp dummyLoc FAdd (TempReg 1) (TempReg 1) (TempReg 2)
                    , IFOp dummyLoc FAdd (TempReg 0) (TempReg 0) (TempReg 1)
                    ]
                )
                `shouldBe` Right
                    ( IntermediateCodeBlock
                        "test"
                        1
                        [ IFMov dummyLoc (TempReg 3) (Imm 0)
                        , IFStore dummyLoc (TempReg 3) StackReg 0
                        , IFMov dummyLoc (TempReg 1) (Imm 1)
                        , IFMov dummyLoc (TempReg 2) (Imm 2)
                        , IFOp dummyLoc FAdd (TempReg 1) (TempReg 1) (TempReg 2)
                        , IFLoad dummyLoc (TempReg 4) StackReg 0
                        , IFOp dummyLoc FAdd (TempReg 5) (TempReg 4) (TempReg 1)
                        , IFStore dummyLoc (TempReg 5) StackReg 0
                        ]
                    )
