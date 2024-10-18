{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import Test.Hspec
import Parser
import Text.Parsec (parse)
import Syntax (UnaryOp(Not, FNeg, Neg))
import Text.Parsec.Pos
import Data.Either (isLeft)

spec :: Spec
spec = do
    describe "parseUnaryOp" $ do
        it "Not" $ do
            parse parseUnaryOp "" "not" `shouldBe` Right (newPos "" 1 1,  Not)
        it "Neg" $ do
            parse parseUnaryOp "" "-" `shouldBe` Right (newPos "" 1 1,  Neg)
        it "FNeg" $ do
            parse parseUnaryOp "" "-." `shouldBe` Right (newPos "" 1 1,  FNeg)
        it "Never" $ do
            parse parseUnaryOp "" "*" `shouldSatisfy` isLeft
