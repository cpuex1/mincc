{-# LANGUAGE OverloadedStrings #-}

module Parser (parseUnaryOp) where

import Syntax
import Text.Parsec
import Text.Parsec.Text (Parser)

parseUnaryOp :: Parser (SourcePos, UnaryOp)
parseUnaryOp = do
    pos <- getPosition
    try (string "not" >> return (pos, Not))
        <|> try (string "-." >> return (pos, FNeg))
        <|> try (string "-" >> return (pos, Neg))
        <?> "unary operator"
