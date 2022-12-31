module Parser.Scheme where

import Types
import Parser.Util

import Data.Char
import Text.Parsec.Char (endOfLine)

import Text.ParserCombinators.Parsec hiding (space, string)
import qualified Text.ParserCombinators.Parsec as P

-- Intraline whitespace.
--
--  <intraline whitespace> → <space or tab>
--
intraSpace :: Parser Char
intraSpace = char ' ' <|> char '\t'

-- Zero or more intraline whitespaces.
intraSpaces :: Parser ()
intraSpaces = skipMany intraSpace

-- Whitespace.
--
--  <whitespace> → <intraline whitespace> | <line ending>
--
-- where
--
--  <line ending> → <newline> | <return> <newline> | <return>
--
space :: Parser Char
space = intraSpace <|> endOfLine <|> char '\r'

-- Zero or more whitespaces.
spaces :: Parser ()
spaces = skipMany space

-- One or more whitespaces.
spaces1 :: Parser()
spaces1 = skipMany1 space

------------------------------------------------------------------------

-- Inline hex escape sequence.
--
--  <inline hex escape> → \x<hex scalar value>;
--
inlineHexEsc :: Parser Char
inlineHexEsc = between (P.string "\\x") (char ';') (fmap chr hex)

-- Explicit sign notation for numeric types.
--
--  <explicit sign> → + | -
--
explicitSign :: Parser Char
explicitSign = char '+' <|> char '-'

-- Initial character for an identifier.
--
--  <initial> → <letter> | <special initial>
--
initial :: Parser Char
initial = letter <|> specialInitial

-- Special initial characters for symbols / identifiers.
--
--  <special initial> → ! | $ | % | & | * | / | : | < | = | > | ? | ^ | _ | ~
--
-- TODO: R⁷RS allows for a Scheme implementation to support
-- additional "extended identifier characters" the set below
-- is just the minimal set of extended identifier characters.
specialInitial :: Parser Char
specialInitial = oneOf "!$%&*+-./:<=>?@^_~"

-- A special subsequent symbol / identifier character.
--
--  <special subsequent> → <explicit sign> | . | @
--
specialSubsequent :: Parser Char
specialSubsequent = explicitSign <|> char '.' <|> char '@'

-- A subsequent character for an identifier.
--
--  <subsequent> → <initial> | <digit> | <special subsequent>
--
subsequent :: Parser Char
subsequent = initial <|> digit <|> specialSubsequent

-- Element of a Scheme symbol / identifier.
--
--  <symbol element> →
--      <any character other than <vertical line> or \>
--      | <inline hex escape> | <mnemonic escape> | \|
--
symbolElement :: Parser Char
symbolElement = noneOf "\\|"
         <|> (try inlineHexEsc)
         <|> mnemonicEsc

-- Parse a Scheme mnemonic escape character.
--
--  <mnemonic escape> → \a | \b | \t | \n | \r
--
mnemonicEsc :: Parser Char
mnemonicEsc = bind "\\a" '\a' -- alarm
          <|> bind "\\b" '\b' -- backspace
          <|> bind "\\t" '\t' -- character tabulation
          <|> bind "\\n" '\n' -- linefeed
          <|> bind "\\r" '\r' -- return

-- Parse a character in a string, including escape sequences.
-- Returns Nothing for escaped newlines.
--
--  <string element> → <any character other than " or \>
--      | <mnemonic escape> | \" | \\
--      | \<intraline whitespace>*<line ending>
--        <intraline whitespace>*
--      | <inline hex escape>
--
stringElement :: Parser (Maybe Char)
stringElement = (fmap Just $ noneOf "\"\\")
            <|> (fmap Just $ try mnemonicEsc)
            <|> (try $ bind "\\\"" (Just '"'))
            <|> (try $ bind "\\\\" (Just '\\'))
            <|> (fmap (const Nothing) $ try (char '\\' >> intraSpaces >> char '\n' >> intraSpaces))
            <|> (fmap Just inlineHexEsc)
-- TODO: Add something along the lines of `choice-try` to clean this up.
-- See the existing `choice` and `try` combinators provided by Parsec.

-- Scheme character name.
--
--  <character name> → alarm | backspace | delete
--      | escape | newline | null | return | space | tab
--
characterName :: Parser Char
characterName = bind "alarm" '\a'
            <|> bind "backspace" '\b'
            <|> bind "delete" '\DEL'
            <|> bind "escape" '\ESC'
            <|> bind "newline" '\n'
            <|> bind "null" '\0'
            <|> bind "return" '\r'
            <|> bind "space" ' '
            <|> bind "tab" '\t'

------------------------------------------------------------------------

-- Scheme identifier or symbol.
--
--  <identifier> → <initial> <subsequent>*
--      | <vertical line> <symbol element>* <vertical line>
--      | <peculiar identifier>
--
identifier :: Parser Sexp
identifier = fmap Id $
    (initial >>= (\i -> fmap ((:) i) $ many subsequent))
        <|> between (char '|') (char '|') (many symbolElement)
        -- TODO: peculiar identifier

-- Scheme boolean
--
--  <boolean> → #t | #f | #true | #false
--
boolean :: Parser Sexp
boolean = char '#' >>
         (fmap (const $ Boolean True)  (P.string "t" <|> P.string "true")
      <|> fmap (const $ Boolean False) (P.string "f" <|> P.string "false"))

-- Scheme character.
--
--  <character> → #\ <any character>
--      | #\ <character name>
--      | #\x<hex scalar value>
--
character :: Parser Sexp
character = fmap Char $
            (try $ P.string "#\\" >> characterName)
        <|> (try $ P.string "#\\x" >> fmap chr hex)
        <|> (try $ P.string "#\\" >> anyChar)

-- A Scheme String.
--
--  <string> → " <string element>* "
--
string :: Parser Sexp
string = fmap Str $
    between (char '"') (char '"') (filterJust <$> many stringElement)

-- TODO: Implement this as a lexme tokenizer / delimiter parser.
parseList :: Parser Sexp
parseList = fmap List $ sepBy parseSexp spaces1

parseSexp :: Parser Sexp
parseSexp = identifier
        <|> string
        <|> between (char '(') (char ')') parseList
