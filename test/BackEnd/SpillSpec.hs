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
 )
import MiddleEnd.Globals (defaultGlobalTable)
import Registers (
    RegOrImm (Imm, Reg),
    RegType (RFloat, RInt),
    savedReg,
    stackReg,
    updateVariant,
 )
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
                    [ IMov dummyLoc (savedReg RInt 0) (Imm 0)
                    , IMov dummyLoc (savedReg RInt 1) (Imm 1)
                    , IMov dummyLoc (savedReg RInt 2) (Imm 2)
                    , IIntOp dummyLoc PAdd (savedReg RInt 1) (savedReg RInt 1) (Reg (savedReg RInt 2))
                    , IIntOp dummyLoc PAdd (savedReg RInt 0) (savedReg RInt 0) (Reg (savedReg RInt 1))
                    ]
                )
                `shouldBe` Right
                    ( IntermediateCodeBlock
                        "test"
                        1
                        [ IMov dummyLoc (savedReg RInt 3) (Imm 0)
                        , IStore dummyLoc (savedReg RInt 3) stackReg 0
                        , IMov dummyLoc (savedReg RInt 1) (Imm 1)
                        , IMov dummyLoc (savedReg RInt 2) (Imm 2)
                        , IIntOp dummyLoc PAdd (savedReg RInt 1) (savedReg RInt 1) (Reg (savedReg RInt 2))
                        , ILoad dummyLoc (savedReg RInt 4) stackReg 0
                        , IIntOp dummyLoc PAdd (savedReg RInt 5) (savedReg RInt 4) (Reg (savedReg RInt 1))
                        , IStore dummyLoc (savedReg RInt 5) stackReg 0
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
                    [ IFMov dummyLoc (savedReg RFloat 0) (Imm 0)
                    , IFMov dummyLoc (savedReg RFloat 1) (Imm 1)
                    , IFMov dummyLoc (savedReg RFloat 2) (Imm 2)
                    , IFOp dummyLoc FAdd (savedReg RFloat 1) (savedReg RFloat 1) (savedReg RFloat 2)
                    , IFOp dummyLoc FAdd (savedReg RFloat 0) (savedReg RFloat 0) (savedReg RFloat 1)
                    ]
                )
                `shouldBe` Right
                    ( IntermediateCodeBlock
                        "test"
                        1
                        [ IFMov dummyLoc (savedReg RFloat 3) (Imm 0)
                        , IFStore dummyLoc (savedReg RFloat 3) stackReg 0
                        , IFMov dummyLoc (savedReg RFloat 1) (Imm 1)
                        , IFMov dummyLoc (savedReg RFloat 2) (Imm 2)
                        , IFOp dummyLoc FAdd (savedReg RFloat 1) (savedReg RFloat 1) (savedReg RFloat 2)
                        , IFLoad dummyLoc (savedReg RFloat 4) stackReg 0
                        , IFOp dummyLoc FAdd (savedReg RFloat 5) (savedReg RFloat 4) (savedReg RFloat 1)
                        , IFStore dummyLoc (savedReg RFloat 5) stackReg 0
                        ]
                    )
