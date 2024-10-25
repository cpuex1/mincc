{-# LANGUAGE OverloadedStrings #-}

module Parser (lexeme, parseLiteral, parseIdent, parsePattern, parseSimpleExpr, parseExpr) where

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

symbol :: Text -> Parser Text
symbol = L.symbol spaced

cSymbol :: Char -> Parser Char
cSymbol = lexeme . char

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
                else return $ RawIdent pos ident
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
    parseNum = parseNumStr >>= \num -> return (read num)

    parseFloat :: Parser Float
    parseFloat =
        try
            ( parseNumStr >>= \num ->
                char '.' >> many digitChar >>= \num2 ->
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
                        many digitChar
                            >>= \num -> return $ "e" ++ unpack sign ++ num
                )

parseRelationBinOp :: Parser BinaryOp
parseRelationBinOp =
    lexeme
        ( try (string "=" >> return (RelationOp Eq))
            <|> try (string "<=" >> return (RelationOp Le))
            <|> try (string ">=" >> return (RelationOp Ge))
            <|> try (string "<>" >> return (RelationOp Ne))
            <|> try (string "<" >> return (RelationOp Lt))
            <|> try (string ">" >> return (RelationOp Gt))
        )
        <?> "a relation binary operator"

parseTermOp :: Parser BinaryOp
parseTermOp =
    lexeme
        ( try (string "+." >> return (FloatOp FAdd))
            <|> try (string "-." >> return (FloatOp FSub))
            <|> try (string "+" >> return (IntOp Add))
            <|> try (string "-" >> return (IntOp Sub))
        )
        <?> "a term operator"

parseFactorOp :: Parser BinaryOp
parseFactorOp =
    lexeme
        ( try (string "*." >> return (FloatOp FMul))
            <|> try (string "/." >> return (FloatOp FDiv))
            <|> try (string "*" >> return (IntOp Mul))
            <|> try (string "/" >> return (IntOp Div))
        )
        <?> "a factor operator"

-- | A pattern parser
parsePattern :: Parser (Pattern RawIdent)
parsePattern =
    lexeme
        ( -- PRec
          try
            ( parseKeyword "rec"
                >> parseIdent
                >>= \ident ->
                    some parseIdent
                        >>= \idents -> return (PRec ident idents)
            )
            -- PTuple
            <|> try
                ( cSymbol '('
                    >> sepBy1 parseIdent (cSymbol ',')
                    >>= \idents -> cSymbol ')' >> return (PTuple idents)
                )
            -- PVar
            <|> try (parseIdent >>= \ident -> return (PVar ident))
        )
        <?> "not a pattern"

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
                        ( between
                            (cSymbol '(')
                            (cSymbol ')')
                            (sepBy1 parseExpr (cSymbol ','))
                            >>= \exprs -> return (PGuard (Tuple pos exprs))
                        )
                    -- Const
                    <|> try (parseLiteral >>= \lit -> return (PGuard (Const pos lit)))
                    -- Var
                    <|> try (parseIdent >>= \ident -> return (PGuard (Var pos ident)))
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
                ( getSourcePos
                    >>= \pos ->
                        cSymbol '.'
                            >> between (cSymbol '(') (cSymbol ')') parseExpr
                            >>= \expr2 -> parseSimpleExpr' (PGuard (Get pos expr1 expr2))
                )
                -- ... or None
                <|> return expr1

-- | Parses a general expression
parseExpr :: Parser ParsedExpr
parseExpr =
    do
        parseExprWithPrecedence 9
        <?> "an expression"
  where
    parseExprWithPrecedence :: Int -> Parser ParsedExpr
    parseExprWithPrecedence precedence
        | precedence > 9 = error "Invalid precedence"
        | precedence == 9 =
            -- Let
            lexeme $
                try
                    ( getSourcePos >>= \pos ->
                        parseKeyword "let"
                            >> parsePattern
                            >>= \pat ->
                                cSymbol '='
                                    >> parseExpr
                                    >>= \value ->
                                        parseKeyword "in"
                                            >> parseExprWithPrecedence 9
                                            >>= \expr -> return (PGuard (Let pos pat (pExp value) (pExp expr)))
                    )
                    <|> parseExprWithPrecedence 8
        | precedence == 8 =
            -- Then
            lexeme $
                try
                    ( getSourcePos >>= \pos ->
                        makeExprParser
                            (parseExprWithPrecedence 7)
                            [
                                [ InfixR
                                    ( cSymbol ';'
                                        >> return (\left right -> PGuard (Let pos PUnit (pExp left) (pExp right)))
                                    )
                                ]
                            ]
                    )
                    <|> parseExprWithPrecedence 7
        | precedence == 7 =
            -- If
            lexeme $
                try
                    ( getSourcePos >>= \pos ->
                        parseKeyword "if"
                            >> parseExprWithPrecedence 6
                            >>= \cond ->
                                parseKeyword "then"
                                    >> parseExprWithPrecedence 6
                                    >>= \then' ->
                                        parseKeyword "else"
                                            >> parseExprWithPrecedence 6
                                            >>= \else' -> return (PGuard (If pos cond (pExp then') (pExp else')))
                    )
                    <|> parseExprWithPrecedence 6
        | precedence == 6 =
            -- Put
            lexeme $
                try
                    ( do
                        pos <- getSourcePos
                        left <- parseSimpleExpr
                        case left of
                            PGuard (Get _ a idx) -> do
                                right <- symbol "<-" >> parseExprWithPrecedence 5
                                return $ PGuard (Put pos a idx right)
                            _ -> fail "not a Put expression"
                    )
                    <|> parseExprWithPrecedence 5
        | precedence == 5 =
            -- RelationBinOp
            lexeme $
                try
                    ( getSourcePos
                        >>= \pos ->
                            makeExprParser
                                (parseExprWithPrecedence 4)
                                [
                                    [ InfixL
                                        ( parseRelationBinOp
                                            >>= \op -> return (\left right -> PGuard (Binary pos op left right))
                                        )
                                    ]
                                ]
                    )
                    <|> parseExprWithPrecedence 4
        | precedence == 4 =
            -- TermOp
            lexeme $
                try
                    ( getSourcePos
                        >>= \pos ->
                            makeExprParser
                                (parseExprWithPrecedence 3)
                                [
                                    [ InfixL
                                        ( parseTermOp
                                            >>= \op -> return (\left right -> PGuard (Binary pos op left right))
                                        )
                                    ]
                                ]
                    )
                    <|> parseExprWithPrecedence 3
        | precedence == 3 =
            -- FactorOp
            lexeme $
                try
                    ( getSourcePos
                        >>= \pos ->
                            makeExprParser
                                (parseExprWithPrecedence 2)
                                [
                                    [ InfixL
                                        ( parseFactorOp
                                            >>= \op -> return (\left right -> PGuard (Binary pos op left right))
                                        )
                                    ]
                                ]
                    )
                    <|> parseExprWithPrecedence 2
        | precedence == 2 =
            getSourcePos >>= \pos ->
                -- FNeg
                lexeme $
                    try
                        ( symbol "-."
                            >> parseExprWithPrecedence 2
                            >>= \expr -> return $ PGuard (Unary pos FNeg expr)
                        )
                        -- Neg
                        <|> try
                            ( cSymbol '-'
                                >> parseExprWithPrecedence 2
                                >>= \expr -> return $ PGuard (Unary pos Neg expr)
                            )
                        <|> parseExprWithPrecedence 1
        | precedence == 1 =
            getSourcePos >>= \pos ->
                -- App
                try
                    ( parseSimpleExpr
                        >>= \func ->
                            some parseSimpleExpr
                                >>= \exprs -> return $ PGuard (App pos func exprs)
                    )
                    -- ArrayCreate
                    <|> try
                        ( parseKeyword "Array.create"
                            >> parseSimpleExpr
                            >>= \expr1 ->
                                parseExprWithPrecedence 1
                                    >>= \expr2 -> return $ PGuard (ArrayCreate pos expr1 expr2)
                        )
                    -- Not
                    <|> try
                        ( parseKeyword "not"
                            >> parseExprWithPrecedence 1
                            >>= \expr -> return $ PGuard (Unary pos Not expr)
                        )
                    <|> parseExprWithPrecedence 0
        | precedence == 0 = parseSimpleExpr
        | otherwise = error "Invalid precedence"
