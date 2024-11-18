{-# LANGUAGE OverloadedStrings #-}

module Parser (lexeme, parseLiteral, parseIdent, parsePattern, parseSimpleExpr, parseExpr) where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Char (isAlphaNum, isDigit)
import Data.Text (Text, cons, unpack)
import GHC.Base (Void)
import Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

{- | Reserved words.
The words "true" and "false" are regarded as literals.
-}
reservedWords :: [Text]
reservedWords = ["let", "in", "rec", "if", "then", "else", "not"]

-- | A space consumer
spaced :: Parser ()
spaced = L.space space1 empty (L.skipBlockCommentNested "(*" "*)")

{- | A lexeme parser.
Consumes trailing white spaces.
-}
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaced

symbol :: Text -> Parser ()
symbol = void . L.symbol spaced

cSymbol :: Char -> Parser ()
cSymbol = lexeme . void . char

-- | An identifier parser
parseIdent :: Parser RawIdent
parseIdent =
    try
        ( lexeme $ do
            pos <- getSourcePos
            firstChar <- lowerChar <|> char '_'
            name <- takeWhileP (Just "an identifier") (\c -> isAlphaNum c || c == '_')
            let ident = cons firstChar name
            if ident `elem` reservedWords
                then fail "reserved word"
                else return $ RawIdent (fromSourcePos pos) ident
        )
        <?> "an identifier"

-- | A specified identifier parser
parseKeyword :: Text -> Parser ()
parseKeyword word =
    lexeme $ string word >> notFollowedBy (alphaNumChar <|> char '_')

-- | A literal parser
parseLiteral :: Parser Literal
parseLiteral =
    lexeme
        ( try (cSymbol '(' >> cSymbol ')' >> return LUnit)
            <|> try (parseKeyword "true" >> return (LBool True))
            <|> try (parseKeyword "false" >> return (LBool False))
            <|> try (parseFloat >>= \num -> return $ LFloat num)
            <|> try (parseNum >>= \num -> return $ LInt num)
        )
        <?> "a literal"
  where
    isNonZero :: Char -> Bool
    isNonZero c = isDigit c && c /= '0'

    parseNumStr :: Parser String
    parseNumStr =
        try (char '0' >> return "0")
            <|> try
                ( do
                    num <- satisfy isNonZero
                    rest <- takeWhileP Nothing isDigit
                    return $ num : unpack rest
                )

    parseNum :: Parser Int
    parseNum =
        read <$> parseNumStr

    parseFloat :: Parser Float
    parseFloat =
        try $
            do
                num <- parseNumStr
                _ <- char '.'
                num2 <- many digitChar
                e <- option "" parseExp
                pure $ read $ (if null num2 then num else num ++ "." ++ num2) ++ e
      where
        parseExp :: Parser String
        parseExp =
            try $
                do
                    _ <- char 'e' <|> char 'E'
                    sign <- option "" (string "+" <|> string "-")
                    num <- many digitChar
                    pure $ "e" ++ unpack sign ++ num

parseRelationBinOp :: Parser (Bool, BinaryOp)
parseRelationBinOp =
    lexeme
        ( try (string "=" >> pure (False, RelationOp Eq))
            <|> try (string "<=" >> pure (True, RelationOp Ge))
            <|> try (string ">=" >> pure (False, RelationOp Ge))
            <|> try (string "<>" >> pure (False, RelationOp Ne))
            <|> try (string "<" >> pure (False, RelationOp Lt))
            <|> try (string ">" >> pure (True, RelationOp Lt))
        )
        <?> "a relation binary operator"

parseTermOp :: Parser BinaryOp
parseTermOp =
    lexeme
        ( try (string "+." >> pure (FloatOp FAdd))
            <|> try (string "-." >> pure (FloatOp FSub))
            <|> try (string "+" >> pure (IntOp Add))
            <|> try (string "-" >> pure (IntOp Sub))
        )
        <?> "a term operator"

parseFactorOp :: Parser BinaryOp
parseFactorOp =
    lexeme
        ( try (string "*." >> pure (FloatOp FMul))
            <|> try (string "/." >> pure (FloatOp FDiv))
            <|> try (string "*" >> pure (IntOp Mul))
            <|> try (string "/" >> pure (IntOp Div))
        )
        <?> "a factor operator"

-- | A pattern parser
parsePattern :: Parser (Pattern RawIdent)
parsePattern =
    lexeme
        ( -- PRec
          try
            ( do
                parseKeyword "rec"
                ident <- parseIdent
                idents <- some parseIdent
                pure (PRec ident idents)
            )
            -- PTuple
            <|> try
                ( do
                    cSymbol '('
                    idents <- sepBy1 parseIdent (cSymbol ',')
                    cSymbol ')'
                    pure (PTuple idents)
                )
            -- PVar
            <|> try (parseIdent >>= \ident -> return (PVar ident))
        )
        <?> "a pattern"

-- | Parses a simple expression
parseSimpleExpr :: Parser ParsedExpr
parseSimpleExpr =
    ( do
        pos <- getSourcePos
        -- Expr with parentheses
        expr <-
            lexeme $
                try (between (cSymbol '(') (cSymbol ')') parseExpr)
                    -- Tuple
                    <|> try
                        ( do
                            exprs <-
                                between
                                    (cSymbol '(')
                                    (cSymbol ')')
                                    (sepBy1 parseExpr (cSymbol ','))
                            pure (PGuard (Tuple (fromSourcePos pos) exprs))
                        )
                    -- Const
                    <|> try (PGuard . Const (fromSourcePos pos) <$> parseLiteral)
                    -- Var
                    <|> try (PGuard . Var (fromSourcePos pos) <$> parseIdent)
        -- Remaining tokens can be components of Get.
        parseSimpleExpr' expr
    )
        <?> "a simple expression"
  where
    -- \| This function was implemented in order to remove a left recursion.
    -- Since `persec` uses recursive decent parsing, the original implementation never halts.
    parseSimpleExpr' :: ParsedExpr -> Parser ParsedExpr
    parseSimpleExpr' expr1 =
        -- Get
        lexeme $
            try
                ( do
                    pos <- getSourcePos
                    cSymbol '.'
                    expr2 <- between (cSymbol '(') (cSymbol ')') parseExpr
                    parseSimpleExpr' (PGuard (Get (fromSourcePos pos) expr1 expr2))
                )
                -- ... or None
                <|> return expr1

-- | Parses a general expression
parseExpr :: Parser ParsedExpr
parseExpr =
    do
        spaced
        parseExprWithPrecedence 9
        <?> "an expression"
  where
    parseExprWithPrecedence :: Int -> Parser ParsedExpr
    parseExprWithPrecedence precedence
        | precedence > 0 =
            lexeme $ try (parseExprWithPrecedence' precedence) <|> parseExprWithPrecedence (precedence - 1)
        | otherwise =
            lexeme $ try (parseExprWithPrecedence' precedence)

    parseExprWithPrecedence' :: Int -> Parser ParsedExpr
    parseExprWithPrecedence' precedence
        | precedence > 9 = error "Invalid precedence"
        | precedence == 9 = do
            -- Let
            pos <- getSourcePos
            parseKeyword "let"
            pat <- parsePattern
            cSymbol '='
            value <- parseExpr
            parseKeyword "in"
            expr <- parseExprWithPrecedence 9
            pure (PGuard (Let (fromSourcePos pos) pat (pExp value) (pExp expr)))
        | precedence == 8 = do
            -- Then
            pos <- getSourcePos
            left <- parseExprWithPrecedence 7
            cSymbol ';'
            PGuard . Let (fromSourcePos pos) PUnit (pExp left) . pExp
                <$> parseExpr
        | precedence == 7 = do
            -- If
            pos <- getSourcePos
            parseKeyword "if"
            cond <- parseExpr
            parseKeyword "then"
            then' <- parseExpr
            parseKeyword "else"
            PGuard . If (fromSourcePos pos) cond (pExp then') . pExp
                <$> parseExpr
        | precedence == 6 = do
            -- Put
            pos <- getSourcePos
            left <- parseSimpleExpr
            case left of
                PGuard (Get _ a idx) -> do
                    right <- symbol "<-" >> parseExprWithPrecedence 5
                    return $ PGuard (Put (fromSourcePos pos) a idx right)
                _ -> fail "a Put expression"
        | precedence == 5 = do
            -- RelationBinOp
            pos <- getSourcePos
            makeExprParser
                (parseExprWithPrecedence 4)
                [
                    [ InfixL
                        ( do
                            (flipped, op) <- parseRelationBinOp
                            if flipped
                                then
                                    return (\left right -> PGuard (Binary (fromSourcePos pos) op right left))
                                else
                                    return (\left right -> PGuard (Binary (fromSourcePos pos) op left right))
                        )
                    ]
                ]
        | precedence == 4 = do
            -- TermOp
            pos <- getSourcePos
            makeExprParser
                (parseExprWithPrecedence 3)
                [
                    [ InfixL
                        ( parseTermOp
                            >>= \op -> return (\left right -> PGuard (Binary (fromSourcePos pos) op left right))
                        )
                    ]
                ]
        | precedence == 3 = do
            -- FactorOp
            pos <- getSourcePos
            makeExprParser
                (parseExprWithPrecedence 2)
                [
                    [ InfixL
                        ( parseFactorOp
                            >>= \op -> return (\left right -> PGuard (Binary (fromSourcePos pos) op left right))
                        )
                    ]
                ]
        | precedence == 2 = do
            pos <- getSourcePos
            -- FNeg
            try
                ( do
                    symbol "-."
                    expr <- parseExprWithPrecedence 2
                    pure $ PGuard (Unary (fromSourcePos pos) FNeg expr)
                )
                -- Neg
                <|> try
                    ( do
                        cSymbol '-'
                        expr <- parseExprWithPrecedence 2
                        pure $ PGuard (Unary (fromSourcePos pos) Neg expr)
                    )
        | precedence == 1 = do
            pos <- getSourcePos
            -- App
            try
                ( do
                    func <- parseSimpleExpr
                    exprs <- some parseSimpleExpr
                    pure $ PGuard (App (fromSourcePos pos) func exprs)
                )
                -- ArrayCreate
                <|> try
                    ( do
                        parseKeyword "Array.create"
                        expr1 <- parseSimpleExpr
                        expr2 <- parseExprWithPrecedence 1
                        pure $ PGuard (ArrayCreate (fromSourcePos pos) expr1 expr2)
                    )
                -- Not
                <|> try
                    ( do
                        parseKeyword "not"
                        expr <- parseExprWithPrecedence 1
                        pure $ PGuard (Unary (fromSourcePos pos) Not expr)
                    )
        | precedence == 0 = parseSimpleExpr
        | otherwise = error "Invalid precedence"
