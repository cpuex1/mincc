{-# LANGUAGE OverloadedStrings #-}

module Parser (parseLiteral, parseUnaryOp) where

import Data.Char (isDigit)
import Syntax
import Text.Parsec
import Text.Parsec.Text (Parser)

-- | A literal parser
parseLiteral :: Parser (SourcePos, LiteralKind)
parseLiteral =
    getPosition >>= \pos ->
        try (char '(' >> spaces >> char ')' >> return (pos, LUnit))
            <|> try (string "true" >> return (pos, LBool True))
            <|> try (string "false" >> return (pos, LBool False))
            <|> try (parseFloat >>= \num -> return (pos, LFloat num))
            <|> try (parseNum >>= \num -> return (pos, LInt num))
            <?> "not a literal"
  where
    isNonZero :: Char -> Bool
    isNonZero c = isDigit c && c /= '0'

    parseNumStr :: Parser String
    parseNumStr =
        try (char '0' >> return "0")
            <|> try
                ( do
                    num <- satisfy isNonZero
                    rest <- many digit
                    return $ num : rest
                )

    parseNum :: Parser Int
    parseNum = parseNumStr >>= \num -> return (read num)

    parseFloat :: Parser Float
    parseFloat =
        try
            ( parseNumStr >>= \num ->
                char '.' >> many digit >>= \num2 ->
                    option "" parseExp >>= \e ->
                        return $ read $ (if null num2 then num else num ++ "." ++ num2) ++ e
            )
      where
        parseExp :: Parser String
        parseExp =
            try
                ( (char 'e' <|> char 'E')
                    >> option "" (string "+" <|> string "-")
                    >>= \sign ->
                        many digit
                            >>= \num -> return $ "e" ++ sign ++ num
                )

-- | A unary operator parser
parseUnaryOp :: Parser (SourcePos, UnaryOp)
parseUnaryOp = do
    pos <- getPosition
    try (string "not" >> return (pos, Not))
        <|> try (string "-." >> return (pos, FNeg))
        <|> try (string "-" >> return (pos, Neg))
        <?> "not an unary operator"
