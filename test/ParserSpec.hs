{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import Test.Hspec
import Parser
import Text.Parsec (parse)
import Syntax
import Text.Parsec.Pos
import Data.Either (isLeft)

spec :: Spec
spec = do
    describe "parseLiteral" $ do
        it "LUnit" $ do
            parse parseLiteral "" "( )" `shouldBe` Right (newPos "" 1 1, LUnit)
        it "LBool" $ do
            parse parseLiteral "" "true" `shouldBe` Right (newPos "" 1 1, LBool True)
        it "LInt" $ do
            parse parseLiteral "" "245" `shouldBe` Right (newPos "" 1 1, LInt 245)
        it "LFloat1" $ do
            parse parseLiteral "" "3.1415" `shouldBe` Right (newPos "" 1 1, LFloat 3.1415)
        it "LFloat2" $ do
            parse parseLiteral "" "123." `shouldBe` Right (newPos "" 1 1, LFloat 123)
        it "LFloat3" $ do
            parse parseLiteral "" "123.E-12" `shouldBe` Right (newPos "" 1 1, LFloat 123e-12)
        it "Never" $ do
            parse parseLiteral "" ".45" `shouldSatisfy` isLeft
    describe "parseUnaryOp" $ do
        it "Not" $ do
            parse parseUnaryOp "" "not" `shouldBe` Right (newPos "" 1 1,  Not)
        it "Neg" $ do
            parse parseUnaryOp "" "-" `shouldBe` Right (newPos "" 1 1,  Neg)
        it "FNeg" $ do
            parse parseUnaryOp "" "-." `shouldBe` Right (newPos "" 1 1,  FNeg)
        it "Never" $ do
            parse parseUnaryOp "" "*" `shouldSatisfy` isLeft
