module Parser.Scheme where

import Types
import Parser.Util

import Data.Char
import Text.ParserCombinators.Parsec

-- Extended identifier characters according to R⁷RS Scheme.
--
-- TODO: R⁷RS allows for a Scheme implementation to support
-- additional "extended identifier characters" the set below
-- is just the minimal set of extended identifier characters.
extChar :: Parser Char
extChar = oneOf "!$%&*+-./:<=>?@^_~"

-- Parse an inline hex escape (e.g. `\x65;`).
hexEsc :: Parser Char
hexEsc = between (string "\\x") (char ';') (fmap chr hex)

-- Parse a Scheme identifier in short form.
parseId' :: Parser Sexp
parseId' = fmap Id $ many1 (letter <|> digit <|> extChar)

-- Parse a Scheme identifier enclosed by ||.
--
-- TODO: || is differnet from any other identifier
parseId'' :: Parser Sexp
parseId'' = fmap Id $ between (char '|') (char '|') (many $ hexEsc <|> noneOf "\\|")

-- Parse a Scheme identifier.
parseId :: Parser Sexp
parseId = parseId'' <|> parseId'

-- Parse a Scheme symbol literal.
parseSymbol :: Parser Sexp
parseSymbol = char '\'' >> parseId

------------------------------------------------------------------------

parseList :: Parser Sexp
parseList = fmap List $ sepBy parseSexp spaces1

parseSexp :: Parser Sexp
parseSexp = parseId
        <|> parseSymbol
        <|> between (char '(') (char ')') parseList
