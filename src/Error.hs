{-# LANGUAGE OverloadedStrings #-}

module Error (CompilerError (ParserError, TypeError, OtherError), displayError) where

import Data.List.NonEmpty (toList)
import Data.Proxy
import qualified Data.Set as S
import Data.Text (Text, intercalate, pack)
import GHC.Base (Void)
import Syntax (Loc (locColumn, locFileName, locLine), fromSourcePos)
import Text.Megaparsec

data CompilerError
    = ParserError (ParseErrorBundle Text Void)
    | TypeError (Maybe Loc) Text
    | OtherError Text
    deriving (Show, Eq)

displayLoc :: Loc -> Text
displayLoc pos =
    " @ " <> locFileName pos <> ":" <> pack (show $ locLine pos) <> ":" <> pack (show $ locColumn pos)

displayError :: CompilerError -> [Text]
displayError err =
    case err of
        ParserError errors ->
            map (formatParserError errors . (\e -> (errorOffset e, displayParserError e))) (Data.List.NonEmpty.toList (bundleErrors errors))
        TypeError (Just pos) msg ->
            ["Type error: " <> msg <> displayLoc pos]
        TypeError Nothing msg ->
            ["Type error: " <> msg]
        OtherError msg ->
            [msg]
  where
    formatParserError :: ParseErrorBundle Text Void -> (Int, Text) -> Text
    formatParserError errors (pos, msg) = "Parser error: " <> msg <> displayLoc position
      where
        position = fromSourcePos $ pstateSourcePos (snd (reachOffset pos (bundlePosState errors)))

    displayParserError :: ParseError Text Void -> Text
    displayParserError (TrivialError _ found expectedTokens) =
        case found of
            (Just item) ->
                "Expected " <> intercalate ", " expectedTokens' <> " but found " <> pack (showErrorItem (Proxy :: Proxy Text) item) <> "."
            Nothing ->
                "Expected " <> intercalate ", " expectedTokens' <> " but not found."
      where
        toTokenList :: S.Set (ErrorItem (Token Text)) -> [ErrorItem (Token Text)]
        toTokenList = S.toAscList

        expectedTokens' = map (pack . showErrorItem (Proxy :: Proxy Text)) $ toTokenList expectedTokens
    displayParserError (FancyError _ failures) =
        intercalate "; " (map pack $ toFailList failures) <> "."
      where
        toFailList :: S.Set (ErrorFancy Void) -> [String]
        toFailList = filterMapFail . S.toAscList
          where
            filterMapFail :: [ErrorFancy Void] -> [String]
            filterMapFail [] = []
            filterMapFail ((ErrorFail reason) : remains) = reason : filterMapFail remains
            filterMapFail (_ : remains) = filterMapFail remains
