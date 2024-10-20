{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import Test.Hspec
import Parser
import Text.Megaparsec (parse, MonadParsec (eof))
import Syntax
import Text.Megaparsec.Pos
import Data.Either (isLeft, isRight)

newPos :: String -> Int -> Int -> SourcePos
newPos source line col = SourcePos source (mkPos line) (mkPos col)

spec :: Spec
spec = do
    describe "parseLiteral" $ do
        it "LUnit" $ do
            parse parseLiteral "" "( )" `shouldBe` Right (newPos "" 1 1, LUnit)
        it "LBool" $ do
            parse parseLiteral "" "true" `shouldBe` Right (newPos "" 1 1, LBool True)
        it "LInt" $ do
            parse parseLiteral "" "123" `shouldBe` Right (newPos "" 1 1, LInt 123)
        it "LFloat1" $ do
            parse parseLiteral "" "3.1415" `shouldBe` Right (newPos "" 1 1, LFloat 3.1415)
        it "LFloat2" $ do
            parse parseLiteral "" "123." `shouldBe` Right (newPos "" 1 1, LFloat 123)
        it "LFloat3" $ do
            parse parseLiteral "" "123.E-12" `shouldBe` Right (newPos "" 1 1, LFloat 123e-12)
        it "Never" $ do
            parse parseLiteral "" ".99" `shouldSatisfy` isLeft
    describe "parseIdent" $ do
        it "Valid" $ do
            parse parseIdent "" "_test314" `shouldBe` Right (newPos "" 1 1, "_test314")
        it "Invalid1" $ do
            parse parseIdent "" "CamelCase" `shouldSatisfy` isLeft
        it "Invalid2" $ do
            parse parseIdent "" "123Doom" `shouldSatisfy` isLeft
        it "Invalid3" $ do
            parse parseIdent "" "rec" `shouldSatisfy` isLeft
    describe "parsePattern" $ do
        it "PRec" $ do
            parse parsePattern "" "rec x y z" `shouldBe` Right (PRec (newPos "" 1 5, "x") [(newPos "" 1 7, "y"), (newPos "" 1 9, "z")])
        it "PTuple" $ do
            parse parsePattern "" "(x, y, z)" `shouldBe` Right (PTuple [(newPos "" 1 2, "x"), (newPos "" 1 5, "y"), (newPos "" 1 8, "z")])
        it "PVar" $ do
            parse parsePattern "" "x" `shouldBe` Right (PVar (newPos "" 1 1, "x"))
        it "Invalid1" $ do
            parse parsePattern "" "rec x" `shouldSatisfy` isLeft
        it "Invalid2" $ do
            parse parsePattern "" "()" `shouldSatisfy` isLeft
    describe "parseLetBinder" $ do
        it "PRec" $ do
            parse parsePattern "" "rec x y z = y + z" `shouldSatisfy` isRight
        it "PTuple" $ do
            parse parsePattern "" "(x, y, z) = (2, 3, 4)" `shouldSatisfy` isRight
        it "PVar" $ do
            parse parsePattern "" "x = f 3" `shouldSatisfy` isRight
        it "Invalid1" $ do
            parse parsePattern "" "rec x = 0" `shouldSatisfy` isLeft
        it "Invalid2" $ do
            parse parsePattern "" "() = ()" `shouldSatisfy` isLeft
    describe "parseSimpleExpr" $ do
        it "Parentheses" $ do
            parse parseSimpleExpr "" "(3 + 4 * (5 + 6))" `shouldSatisfy` isRight
        it "Tuple" $ do
            parse parseSimpleExpr "" "(2, f 3, 4 * 5)" `shouldSatisfy` isRight
        it "Var" $ do
            parse parseSimpleExpr "" "ident314" `shouldSatisfy` isRight
        it "Get" $ do
            parse parseSimpleExpr "" "ident314.(foo).(bar)" `shouldSatisfy` isRight
        it "Invalid1" $ do
            parse parseSimpleExpr "" "( 3 * 4 + 5" `shouldSatisfy` isLeft
        it "Invalid2" $ do
            parse (parseSimpleExpr >>= \expr -> eof >> return expr) "" "314ident" `shouldSatisfy` isLeft
