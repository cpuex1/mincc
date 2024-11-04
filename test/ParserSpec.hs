{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import Data.Either (isLeft, isRight)
import Parser
import Syntax
import Test.Hspec
import Text.Megaparsec (MonadParsec (eof), parse)
import Text.Megaparsec.Pos

newPos :: String -> Int -> Int -> SourcePos
newPos source line col = SourcePos source (mkPos line) (mkPos col)

spec :: Spec
spec = do
    describe "parseLiteral" $ do
        it "LUnit" $ do
            parse parseLiteral "" "( )" `shouldBe` Right LUnit
        it "LBool" $ do
            parse parseLiteral "" "true" `shouldBe` Right (LBool True)
        it "LInt" $ do
            parse parseLiteral "" "123" `shouldBe` Right (LInt 123)
        it "LFloat1" $ do
            parse parseLiteral "" "3.1415" `shouldBe` Right (LFloat 3.1415)
        it "LFloat2" $ do
            parse parseLiteral "" "123." `shouldBe` Right (LFloat 12345)
        it "LFloat3" $ do
            parse parseLiteral "" "123.E-12" `shouldBe` Right (LFloat 123e-12)
        it "Invalid1" $ do
            parse parseLiteral "" ".99" `shouldSatisfy` isLeft
        it "Invalid2" $ do
            parse parseLiteral "" "trued" `shouldSatisfy` isLeft
    describe "parseIdent" $ do
        it "Valid" $ do
            parse parseIdent "" "_test314" `shouldBe` Right (RawIdent (newPos "" 1 1) "_test314")
        it "Invalid1" $ do
            parse parseIdent "" "CamelCase" `shouldSatisfy` isLeft
        it "Invalid2" $ do
            parse parseIdent "" "123Doom" `shouldSatisfy` isLeft
        it "Invalid3" $ do
            parse parseIdent "" "rec" `shouldSatisfy` isLeft
    describe "parsePattern" $ do
        it "PRec" $ do
            parse parsePattern "" "rec x y z" `shouldSatisfy` isRight
        it "PTuple" $ do
            parse parsePattern "" "(x , y, z )" `shouldSatisfy` isRight
        it "PVar" $ do
            parse parsePattern "" "x" `shouldSatisfy` isRight
        it "Invalid1" $ do
            parse parsePattern "" "rec x" `shouldSatisfy` isLeft
        it "Invalid2" $ do
            parse parsePattern "" "()" `shouldSatisfy` isLeft
    describe "parseLetBinder" $ do
        it "PRec" $ do
            parse parsePattern "" "rec x y z = y + z" `shouldSatisfy` isRight
        it "PTuple" $ do
            parse parsePattern "" "( x , y,z) = (2, 3, 4)" `shouldSatisfy` isRight
        it "PVar" $ do
            parse parsePattern "" "x = f 3" `shouldSatisfy` isRight
        it "Invalid1" $ do
            parse parsePattern "" "rec x = 0" `shouldSatisfy` isLeft
        it "Invalid2" $ do
            parse parsePattern "" "() = ()" `shouldSatisfy` isLeft
    describe "parseSimpleExpr" $ do
        it "Parentheses" $ do
            parse parseSimpleExpr "" "(3 +4 * ( 5+6))" `shouldSatisfy` isRight
        it "Tuple" $ do
            parse parseSimpleExpr "" "(2, f 3, 4 * 5)" `shouldSatisfy` isRight
        it "Var" $ do
            parse parseSimpleExpr "" "ident314" `shouldSatisfy` isRight
        it "Get" $ do
            parse parseSimpleExpr "" "ident314 . (foo).(bar)" `shouldSatisfy` isRight
        it "Invalid1" $ do
            parse parseSimpleExpr "" "( 3 * 4 + 5" `shouldSatisfy` isLeft
        it "Invalid2" $ do
            parse (parseSimpleExpr <* eof) "" "314ident" `shouldSatisfy` isLeft
    describe "parseExpr" $ do
        it "Let" $ do
            parse parseExpr "" "let x = let y=15 in y in x" `shouldSatisfy` isRight
        it "Then" $ do
            parse parseExpr "" "314; x; f y" `shouldSatisfy` isRight
        it "If" $ do
            parse parseExpr "" "if x = 3 then y else z" `shouldSatisfy` isRight
        it "Put" $ do
            parse (parseExpr <* eof) "" "array.(x) <- 123 + y" `shouldSatisfy` isRight
        it "RelationBinOp1" $ do
            parse parseExpr "" "x < 314 <> true" `shouldSatisfy` isRight
        it "RelationBinOp2" $ do
            parse parseExpr "" "x = 3" `shouldSatisfy` isRight
        it "TermOp" $ do
            parse parseExpr "" "x + 314 * y" `shouldSatisfy` isRight
        it "FactorOp" $ do
            parse parseExpr "" "314 * x * y" `shouldSatisfy` isRight
        it "FNeg" $ do
            parse parseExpr "" "-. 3.1415" `shouldSatisfy` isRight
        it "Neg" $ do
            parse parseExpr "" "-123" `shouldSatisfy` isRight
        it "App" $ do
            parse parseExpr "" "f x y (g a b)" `shouldSatisfy` isRight
        it "ArrayCreate" $ do
            parse parseExpr "" "Array.create 3 x" `shouldSatisfy` isRight
        it "Not" $ do
            parse parseExpr "" "not true" `shouldSatisfy` isRight
