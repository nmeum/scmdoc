module Util where

import SchemeDoc
import SchemeDoc.Parser.R7RS
import qualified Text.ParserCombinators.Parsec as P

parseErrors :: P.Parser a -> String -> String
parseErrors p input =
    case P.parse p "" input of
        Left err ->
            last $ lines $ show err
        Right _  -> ""

parse :: String -> Either P.ParseError [Sexp]
parse = P.parse scheme ""
