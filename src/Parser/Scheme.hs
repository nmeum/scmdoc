module Parser.Scheme where

import Types
import Parser.Util

import Data.Char
import Text.ParserCombinators.Parsec

-- Intraline whitespace.
intraSpace :: Parser Char
intraSpace = char ' ' <|> char '\t'

-- Zero or more intraline whitespaces.
intraSpaces :: Parser ()
intraSpaces = skipMany intraSpace

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

-- Parse an escapes sequence within a string.
-- Returns `Nothing` for escaped newlines.
stringEsc :: Parser (Maybe Char)
stringEsc = char '\\' >> (fmap (\_ -> Nothing) nlEsc <|> fmap Just escChr)
    where
        nlEsc  = intraSpaces >> char '\n' >> intraSpaces
        escChr = bind '\a'  'a' -- alarm
             <|> bind '\b'  'b' -- backspace
             <|> bind '\t'  't' -- character tabulation
             <|> bind '\n'  'n' -- linefeed
             <|> bind '\r'  'r' -- return
             <|> bind '"'  '"'  -- double quote
             <|> bind '\\' '\\' -- backslash
             <|> bind '|'  '|'  -- vertical line
             <|> fail "unknown escape sequence"

-- Parse a character in a string, including escape sequences.
stringChar :: Parser (Maybe Char)
stringChar = (try stringEsc)
         <|> Just <$> ((try hexEsc) <|> noneOf "\"")

------------------------------------------------------------------------

parseId :: Parser Sexp
parseId = parseId'' <|> parseId'

parseSymbol :: Parser Sexp
parseSymbol = char '\'' >> parseId

parseString :: Parser Sexp
parseString = fmap Str $
    between (char '"') (char '"') (filterJust <$> many stringChar)

parseList :: Parser Sexp
parseList = fmap List $ sepBy parseSexp spaces1

parseSexp :: Parser Sexp
parseSexp = parseId
        <|> parseSymbol
        <|> parseString
        <|> between (char '(') (char ')') parseList
