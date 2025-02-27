{-# LANGUAGE OverloadedStrings #-}

module FrontEnd.Parser (
    lexeme,
    parseLiteral,
    parseIdent,
    parsePattern,
    parseSimpleExpr,
    parseExpr,
    parsePartialExpr,
) where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Char (isAlphaNum, isDigit)
import Data.Functor ((<&>))
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

-- | Gets the source location.
getSourceLoc :: (TraversableStream s, MonadParsec e s m) => m Loc
getSourceLoc = getSourcePos <&> fromSourcePos

-- | An identifier parser
parseIdent :: Parser RawIdent
parseIdent =
    lexeme
        ( try
            ( do
                -- Can backtrack due to the existence of reserved words.
                pos <- getSourceLoc
                firstChar <- lowerChar <|> char '_'
                name <- takeWhileP (Just "an identifier") (\c -> isAlphaNum c || c == '_')
                let ident = cons firstChar name
                if ident `elem` reservedWords
                    then fail "a reserved word"
                    else return $ RawIdent pos ident
            )
            <?> "an identifier"
        )

-- | A specified identifier parser
parseKeyword :: Text -> Parser ()
parseKeyword word =
    lexeme $ string word >> notFollowedBy (alphaNumChar <|> char '_')

-- | A literal parser
parseLiteral :: Parser RLiteral
parseLiteral =
    lexeme
        ( (cSymbol '(' >> cSymbol ')' >> return LUnit)
            <|> (parseKeyword "true" >> return (LBool True))
            <|> (parseKeyword "false" >> return (LBool False))
            -- Can consume a numeric.
            <|> try (parseFloat >>= \num -> return $ LFloat num)
            <|> (parseNum >>= \num -> return $ LInt num)
        )
        <?> "a literal"
  where
    isNonZero :: Char -> Bool
    isNonZero c = isDigit c && c /= '0'

    parseNumStr :: Parser String
    parseNumStr =
        try (char '0' >> return "0")
            <|> ( do
                    num <- satisfy isNonZero
                    rest <- takeWhileP Nothing isDigit
                    return $ num : unpack rest
                )

    parseNum :: Parser Int
    parseNum =
        read <$> parseNumStr

    parseFloat :: Parser Float
    parseFloat = do
        num <- parseNumStr
        _ <- char '.'
        num2 <- many digitChar
        e <- option "" parseExp
        pure $ (read :: String -> Float) $ (if null num2 then num else num ++ "." ++ num2) ++ e
      where
        parseExp :: Parser String
        parseExp = do
            _ <- char 'e' <|> char 'E'
            sign <- option "" (string "+" <|> string "-")
            num <- many digitChar
            pure $ "e" ++ unpack sign ++ num

parseRelationBinOp :: Parser (Bool, BinaryOp)
parseRelationBinOp =
    lexeme
        ( (string "=" >> pure (False, RelationOp Eq))
            <|> (string "<=" >> pure (True, RelationOp Ge))
            <|> (string ">=" >> pure (False, RelationOp Ge))
            <|> (string "<>" >> pure (False, RelationOp Ne))
            <|> try (string "<" >> notFollowedBy "-" >> pure (False, RelationOp Lt))
            <|> (string ">" >> pure (True, RelationOp Lt))
        )
        <?> "a relation binary operator"

parseTermOp :: Parser BinaryOp
parseTermOp =
    lexeme
        ( (string "+." >> pure (FloatOp FAdd))
            <|> (string "-." >> pure (FloatOp FSub))
            <|> (string "+" >> pure (IntOp Add))
            <|> (string "-" >> pure (IntOp Sub))
        )
        <?> "a term operator"

parseFactorOp :: Parser BinaryOp
parseFactorOp =
    lexeme
        ( (string "*." >> pure (FloatOp FMul))
            <|> (string "/." >> pure (FloatOp FDiv))
            <|> (string "*" >> pure (IntOp Mul))
            <|> (string "/" >> pure (IntOp Div))
        )
        <?> "a factor operator"

-- | A pattern parser
parsePattern :: Parser (Pattern RawIdent)
parsePattern =
    lexeme
        ( -- PRec
          ( do
                try $ parseKeyword "rec"
                ident <- parseIdent
                args <- some parseIdent
                pure (PRec ident args)
          )
            -- PTuple
            <|> ( do
                    cSymbol '('
                    values <- sepBy1 parseIdent (cSymbol ',')
                    cSymbol ')'
                    pure (PTuple values)
                )
            -- PVar
            <|> (parseIdent >>= \ident -> return (PVar ident))
        )
        <?> "a pattern"

-- | Parses a simple expression
parseSimpleExpr :: Parser ParsedExpr
parseSimpleExpr =
    ( do
        pos <- getSourceLoc
        expr <-
            lexeme $
                -- Expr with parentheses
                try (between (cSymbol '(') (cSymbol ')') parseExpr)
                    -- Tuple
                    -- Can backtrack due to the existence of unit.
                    <|> try
                        ( do
                            exprs <-
                                between
                                    (cSymbol '(')
                                    (cSymbol ')')
                                    (sepBy1 parseExpr (cSymbol ','))
                            pure (Tuple pos exprs)
                        )
                    -- Const
                    <|> Const pos
                    <$> parseLiteral
                        -- Var
                        <|> Var pos
                    <$> parseIdent
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
            ( do
                pos <- getSourceLoc
                cSymbol '.'
                expr2 <- between (cSymbol '(') (cSymbol ')') parseExpr
                parseSimpleExpr' (Get pos expr1 expr2)
            )
                -- ... or None
                <|> return expr1

-- | Parses an expression related to function application.
parseAppExpr :: Parser ParsedExpr
parseAppExpr = do
    pos <- getSourceLoc
    lexeme
        ( try
            ( do
                -- App
                -- Can backtrack.
                func <- parseSimpleExpr
                args <- some parseSimpleExpr
                pure $ App pos func args
            )
            <|> ( do
                    -- ArrayCreate
                    parseKeyword "Array.create" <|> parseKeyword "Array.make"
                    firstArg <- parseSimpleExpr
                    ArrayCreate pos firstArg <$> parseSimpleExpr
                )
            <|> ( do
                    -- Not
                    parseKeyword "not"
                    Unary pos Not <$> parseSimpleExpr
                )
            -- A simple expression
            <|> parseSimpleExpr
            <?> "an expression with function application"
        )

-- | Parses an expression related to operators.
parseOperatorExpr :: Parser ParsedExpr
parseOperatorExpr = do
    lexeme
        ( makeExprParser
            parseAppExpr
            [ -- UnaryOp

                [ Prefix
                    ( do
                        pos <- getSourceLoc
                        symbol "-."
                        pure $ Unary pos FNeg
                    )
                , Prefix
                    ( do
                        pos <- getSourceLoc
                        symbol "-"
                        pure $ Unary pos Neg
                    )
                ]
            , -- FactorOp

                [ InfixL
                    ( do
                        pos <- getSourceLoc
                        Binary pos <$> parseFactorOp
                    )
                ]
            , -- TermOp

                [ InfixL
                    ( do
                        pos <- getSourceLoc
                        Binary pos <$> parseTermOp
                    )
                ]
            , -- RelationBinOp

                [ InfixL
                    ( do
                        pos <- getSourceLoc
                        (flipped, op) <- parseRelationBinOp
                        if flipped
                            then
                                pure (flip (Binary pos op))
                            else
                                pure (Binary pos op)
                    )
                ]
            ]
        )
        <?> "an expression with operators"

-- | Parses an expression which cannot be split by ";".
parseThenExpr :: Parser ParsedExpr
parseThenExpr =
    lexeme
        ( ( do
                -- If
                pos <- getSourceLoc
                parseKeyword "if"
                cond <- parseExpr'
                parseKeyword "then"
                then' <- parseExpr'
                parseKeyword "else"
                If pos (CIdentity cond) then' <$> parseExpr'
          )
            <|> ( do
                    pos <- getSourceLoc
                    expr <- parseOperatorExpr
                    case expr of
                        (Get _ array idx) ->
                            ( do
                                -- Put
                                symbol "<-"
                                Put pos array idx <$> parseExpr'
                            )
                                <|>
                                -- An expression related to operators
                                pure expr
                        _ ->
                            -- An expression related to operators
                            pure expr
                )
            <?> "an \"if\" expression or \"<-\" expression"
        )

-- | Parses a let expression partially
parseLetHeader :: Parser (Loc, Pattern RawIdent, ParsedExpr)
parseLetHeader = do
    pos <- getSourceLoc
    parseKeyword "let"
    pat <- parsePattern
    cSymbol '='
    value <- parseExpr
    parseKeyword "in"
    pure (pos, pat, value)

-- | Parses a let expression
parseLetExpr :: Parser ParsedExpr
parseLetExpr = do
    (pos, pat, value) <- parseLetHeader
    Let pos pat value <$> parseExpr

-- | Parses a general expression without ";" at the top.
parseExpr' :: Parser ParsedExpr
parseExpr' = parseLetExpr <|> parseThenExpr

-- | Parses a general expression
parseExpr :: Parser ParsedExpr
parseExpr =
    do
        spaced
        lexeme
            ( -- Let
              parseLetExpr
                <|> ( do
                        pos <- getSourceLoc
                        expr <- parseThenExpr
                        ( do
                                -- Then
                                cSymbol ';'
                                Let pos PUnit expr
                                    <$> parseExpr
                            )
                            <|>
                            -- A then expression
                            pure expr
                    )
                <?> "an expression"
            )

-- | Parses a partial let expression
parsePartialExpr :: ParsedExpr -> Parser ParsedExpr
parsePartialExpr body =
    do
        spaced
        lexeme
            ( do
                -- Let
                (pos, pat, value) <- parseLetHeader
                body' <-
                    parsePartialExpr body
                        <|> pure body
                pure $ Let pos pat value body'
            )
