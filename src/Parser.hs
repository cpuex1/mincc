{-# LANGUAGE OverloadedStrings #-}

module Parser (parseLiteral, parseIdent, parsePattern, parseLetBinder, parseSimpleExpr, parseExpr) where

import Control.Monad.Combinators.Expr
import Data.Char (isDigit)
import Data.Text (Text, cons, pack, unpack)
import Syntax
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec (ErrorFancy Text) Text

{- | Reserved words.
The words "true" and "false" are regarded as literals.
-}
reservedWords :: [Text]
reservedWords = ["let", "in", "rec", "if", "then", "else", "not"]

-- | A literal parser
parseLiteral :: Parser Literal
parseLiteral =
    getSourcePos >>= \pos ->
        try (char '(' >> space >> char ')' >> return (pos, LUnit))
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
                    rest <- many digitChar
                    return $ num : rest
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

-- | An identifier parser
parseIdent :: Parser Ident
parseIdent =
    try
        ( do
            pos <- getSourcePos
            c <- lowerChar <|> char '_'
            name <- many (digitChar <|> letterChar <|> char '_')
            let ident = cons c $ pack name
            if ident `elem` reservedWords
                then customFailure $ ErrorCustom "reserved word"
                else return (pos, ident)
        )
        <?> "not an identifier"

parseRelationBinOp :: Parser BinaryOp
parseRelationBinOp =
    try (string "=" >> return (RelationOp Eq))
        <|> try (string "<=" >> return (RelationOp Le))
        <|> try (string ">=" >> return (RelationOp Ge))
        <|> try (string "<>" >> return (RelationOp Ne))
        <|> try (string "<" >> return (RelationOp Lt))
        <|> try (string ">" >> return (RelationOp Gt))
        <?> "not a relation binary operator"

parseTermOp :: Parser BinaryOp
parseTermOp =
    try (string "+." >> return (FloatOp FAdd))
        <|> try (string "-." >> return (FloatOp FSub))
        <|> try (string "+" >> return (IntOp Add))
        <|> try (string "-" >> return (IntOp Sub))
        <?> "not a term operator"

parseFactorOp :: Parser BinaryOp
parseFactorOp =
    try (string "*." >> return (FloatOp FMul))
        <|> try (string "/." >> return (FloatOp FDiv))
        <|> try (string "*" >> return (IntOp Mul))
        <|> try (string "/" >> return (IntOp Div))
        <?> "not a factor operator"

-- | A pattern parser
parsePattern :: Parser Pattern
parsePattern =
    -- PRec
    try
        ( string "rec"
            >> space1
            >> parseIdent
            >>= \ident ->
                some (try (space1 >> parseIdent))
                    >>= \idents -> return (PRec ident idents)
        )
        -- PTuple
        <|> try
            ( char '('
                >> sepBy1 parseIdent (space >> char ',' >> space)
                >>= \idents -> char ')' >> return (PTuple idents)
            )
        -- PVar
        <|> try (parseIdent >>= \ident -> return (PVar ident))
        <?> "not a pattern"

-- | A let binder parser
parseLetBinder :: Parser LetBinder
parseLetBinder =
    parsePattern
        >>= \pat ->
            space
                >> char '='
                >> space
                >> parseExpr
                >>= \value -> return $ LetBinder pat value

-- | Parses a simple expression
parseSimpleExpr :: Parser Expr
parseSimpleExpr = do
    pos <- getSourcePos
    -- Expr with parentheses
    expr <-
        try (char '(' >> space >> parseExpr >>= \expr -> space >> char ')' >> return expr)
            -- Tuple
            <|> try
                ( char '('
                    >> sepBy1 parseExpr (try (space >> char ',' >> space))
                    >>= \exprs -> char ')' >> return (pos, Tuple exprs)
                )
            -- Const
            <|> try (parseLiteral >>= \lit -> return (pos, Const lit))
            -- Var
            <|> try (parseIdent >>= \ident -> return (pos, Var ident))
    -- Remaining tokens can be components of Get.
    parseSimpleExpr' expr
  where
    -- \| This function was implemented in order to remove a left recursion.
    -- Since `persec` uses recursive decent parsing, the original implementation never halts.
    parseSimpleExpr' :: Expr -> Parser Expr
    parseSimpleExpr' expr1 = do
        -- Get
        try
            ( getSourcePos
                >>= \pos ->
                    char '.'
                        >> space
                        >> char '('
                        >> space
                        >> parseExpr
                        >>= \expr2 ->
                            space
                                >> char ')'
                                >> parseSimpleExpr' (pos, Get expr1 expr2)
            )
            -- ... or None
            <|> return expr1

-- | Parses a general expression
parseExpr :: Parser Expr
parseExpr = do
    parseExprWithPrecedence 9
  where
    parseExprWithPrecedence :: Int -> Parser Expr
    parseExprWithPrecedence precedence
        | precedence > 9 = error "Invalid precedence"
        | precedence == 9 =
            -- Let
            try
                ( getSourcePos >>= \pos ->
                    string "let"
                        >> space
                        >> parseLetBinder
                        >>= \binder ->
                            space
                                >> string "in"
                                >> space
                                >> parseExprWithPrecedence 9
                                >>= \expr -> return (pos, Let binder expr)
                )
                <|> parseExprWithPrecedence 8
        | precedence == 8 =
            -- Then
            try
                ( getSourcePos >>= \pos ->
                    makeExprParser
                        (parseExprWithPrecedence 7)
                        [
                            [ InfixR
                                ( space
                                    >> char ';'
                                    >> space
                                    >> return (\left right -> (pos, Then left right))
                                )
                            ]
                        ]
                )
                <|> parseExprWithPrecedence 7
        | precedence == 7 =
            -- If
            try
                ( getSourcePos >>= \pos ->
                    string "if"
                        >> space
                        >> parseExprWithPrecedence 7
                        >>= \cond ->
                            space
                                >> string "then"
                                >> space
                                >> parseExprWithPrecedence 7
                                >>= \then' ->
                                    space
                                        >> string "else"
                                        >> space
                                        >> parseExprWithPrecedence 7
                                        >>= \else' -> return (pos, If cond then' else')
                )
                <|> parseExprWithPrecedence 6
        | precedence == 6 =
            -- Set
            try
                ( getSourcePos
                    >>= \pos ->
                        parseSimpleExpr
                            >>= \left ->
                                space
                                    >> string "<-"
                                    >> space
                                    >> parseExprWithPrecedence 5
                                    >>= \right -> return (pos, Set left right)
                )
                <|> parseExprWithPrecedence 5
        | precedence == 5 =
            -- RelationBinOp
            try
                ( getSourcePos
                    >>= \pos ->
                        makeExprParser
                            (parseExprWithPrecedence 4)
                            [
                                [ InfixL
                                    ( space
                                        >> parseRelationBinOp
                                        >>= \op -> space >> return (\left right -> (pos, Binary op left right))
                                    )
                                ]
                            ]
                )
                <|> parseExprWithPrecedence 4
        | precedence == 4 =
            -- TermOp
            try
                ( getSourcePos
                    >>= \pos ->
                        makeExprParser
                            (parseExprWithPrecedence 3)
                            [
                                [ InfixL
                                    ( space
                                        >> parseTermOp
                                        >>= \op -> space >> return (\left right -> (pos, Binary op left right))
                                    )
                                ]
                            ]
                )
                <|> parseExprWithPrecedence 3
        | precedence == 3 =
            -- FactorOp
            try
                ( getSourcePos
                    >>= \pos ->
                        makeExprParser
                            (parseExprWithPrecedence 2)
                            [
                                [ InfixL
                                    ( space
                                        >> parseFactorOp
                                        >>= \op -> space >> return (\left right -> (pos, Binary op left right))
                                    )
                                ]
                            ]
                )
                <|> parseExprWithPrecedence 2
        | precedence == 2 =
            getSourcePos >>= \pos ->
                -- FNeg
                try
                    ( string "-."
                        >> space
                        >> parseExprWithPrecedence 2
                        >>= \expr -> return (pos, Unary FNeg expr)
                    )
                    -- Neg
                    <|> try
                        ( char '-'
                            >> space
                            >> parseExprWithPrecedence 2
                            >>= \expr -> return (pos, Unary Neg expr)
                        )
                    <|> parseExprWithPrecedence 1
        | precedence == 1 =
            getSourcePos >>= \pos ->
                -- App
                try
                    ( parseSimpleExpr
                        >>= \func ->
                            many (space1 >> parseSimpleExpr)
                                >>= \exprs -> return (pos, App func exprs)
                    )
                    -- ArrayMake
                    <|> try
                        ( string "Array.create"
                            >> space
                            >> parseSimpleExpr
                            >>= \expr1 ->
                                space
                                    >> parseExprWithPrecedence 1
                                    >>= \expr2 -> return (pos, ArrayMake expr1 expr2)
                        )
                    -- Not
                    <|> try
                        ( string "not"
                            >> space
                            >> parseExprWithPrecedence 1
                            >>= \expr -> return (pos, Unary Not expr)
                        )
                    <|> parseExprWithPrecedence 0
        | precedence == 0 = parseSimpleExpr
        | otherwise = error "Invalid precedence"
