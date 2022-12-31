module Parser.Scheme where

import Types
import Parser.Util

import Data.Char
import Text.Parsec.Char (endOfLine)
import Text.ParserCombinators.Parsec hiding (space)

-- Intraline whitespace as defined in formal R⁷RS syntax.
intraSpace :: Parser Char
intraSpace = char ' ' <|> char '\t'

-- Zero or more intraline whitespaces.
intraSpaces :: Parser ()
intraSpaces = skipMany intraSpace

-- Whitespace as defined in formal R⁷RS syntax.
space :: Parser Char
space = intraSpace <|> endOfLine <|> char '\r'

-- Zero or more whitespaces.
spaces :: Parser ()
spaces = skipMany space

-- One or more whitespaces.
spaces1 :: Parser()
spaces1 = skipMany1 space

------------------------------------------------------------------------

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

explicitSign :: Parser Char
explicitSign = char '+' <|> char '-'

-- Parse a Scheme identifier without enclosing vertical lines.
parseId' :: Parser Sexp
parseId' = do
    first  <- initial
    subseq <- many (initial <|> digit <|> specSubseq)
    return $ Id (first : subseq)
    where
        initial = letter <|> extChar
        specSubseq = explicitSign <|> char '.' <|> char '@'

-- Parse a Scheme identifier enclosed by ||.
--
-- TODO: || is differnet from any other identifier
-- TODO: Support peculiar identifier
parseId'' :: Parser Sexp
parseId'' = fmap Id $ between (char '|') (char '|') (many symbolElem)

symbolElem :: Parser Char
symbolElem = noneOf "\\|" <|> (try hexEsc) <|> (char '\\' >> mnemonicEsc)

-- Parse a Scheme mnemonic escape character as defined in the formal syntax.
mnemonicEsc :: Parser Char
mnemonicEsc = bind '\a'  'a' -- alarm
          <|> bind '\b'  'b' -- backspace
          <|> bind '\t'  't' -- character tabulation
          <|> bind '\n'  'n' -- linefeed
          <|> bind '\r'  'r' -- return

-- Parse an escapes sequence within a string.
-- Returns `Nothing` for escaped newlines.
stringEsc :: Parser (Maybe Char)
stringEsc = char '\\' >> (fmap (\_ -> Nothing) nlEsc <|> fmap Just escChr)
    where
        nlEsc  = intraSpaces >> char '\n' >> intraSpaces
        escChr = mnemonicEsc
             <|> bind '"'  '"'  -- double quote
             <|> bind '\\' '\\' -- backslash
             <|> fail "unknown escape sequence"

-- Parse a character in a string, including escape sequences.
stringElem :: Parser (Maybe Char)
stringElem = (try stringEsc)
         <|> Just <$> ((try hexEsc) <|> noneOf "\"\\")

------------------------------------------------------------------------

parseId :: Parser Sexp
parseId = parseId'' <|> parseId'

parseSymbol :: Parser Sexp
parseSymbol = char '\'' >> parseId

parseString :: Parser Sexp
parseString = fmap Str $
    between (char '"') (char '"') (filterJust <$> many stringElem)

parseList :: Parser Sexp
parseList = fmap List $ sepBy parseSexp spaces1

parseSexp :: Parser Sexp
parseSexp = parseId
        <|> parseSymbol
        <|> parseString
        <|> between (char '(') (char ')') parseList
