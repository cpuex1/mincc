{-# LANGUAGE OverloadedStrings #-}

module TypeInferrerSpec (spec) where

import Control.Monad.Except
import Control.Monad.State
import Syntax
import Test.Hspec
import TypeInferrer

spec :: Spec
spec = do
    describe "genNewId" $ do
        it "Test1" $ do
            evalState (runExceptT genNewId) defaultEnv `shouldBe` Right 0
        it "Test2" $ do
            evalState
                ( runExceptT $ do
                    _ <- genNewId
                    _ <- genNewId
                    _ <- genNewId
                    gets assigned
                )
                defaultEnv
                `shouldBe` Right 3
        it "Test3" $ do
            evalState
                ( runExceptT $ do
                    _ <- genNewId
                    _ <- genNewId
                    _ <- genNewId
                    t <- gets table
                    pure $ length t
                )
                defaultEnv
                `shouldBe` Right 3
    describe "registerIdent" $ do
        it "Test1" $ do
            let x = UserDefined (Loc "" 1 1) "x"
             in let y = UserDefined (Loc "" 1 1) "y"
                 in evalState
                        ( runExceptT $
                            do
                                registerIdent x
                                registerIdent y
                                get
                        )
                        defaultEnv
                        `shouldBe` Right (TypeEnv 2 [Nothing, Nothing] [(y, 1), (x, 0)])
