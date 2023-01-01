module SchemeDoc.Parser.R7RS (scheme) where

import SchemeDoc
import SchemeDoc.Parser.Util

import Data.Char

import Text.Parsec.Char (endOfLine)
import Text.ParserCombinators.Parsec hiding (space, spaces, string)
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

-- Parse a Scheme delimiter.
--
--  <delimiter> → <whitespace> | <vertical line> | ( | ) | " | ;
--
-- Does not consume any input on failure.
delimiter :: Parser ()
delimiter = skip $ space <|> char '|' <|> char '(' <|> char ')' <|> char '"' <|> char ';'

-- Scheme block comment.
--
--  <nested comment> → #| <comment text>
--                      <comment cont>* |#
--
-- XXX: Not sure if I understood the nested comment grammar rule correctly.
nestedComment :: Parser String
nestedComment = P.string "#|" >> manyTill anyChar (P.string "|#")

------------------------------------------------------------------------

-- Source code comment.
--
--  <comment> → ; <all subsequent characters up to a
--      line ending>
--      | <nested comment>
--      | #; <intertoken space> <datum>
--
comment :: Parser Sexp
comment = fmap (Comment . unwords . words) $
          (char ';' >> manyTill anyChar endOfLine)
           <|> nestedComment
      -- TODO: #; comments

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
         (fmap (const $ Boolean True)  ((try $ P.string "true") <|> P.string "t")
      <|> fmap (const $ Boolean False) ((try $ P.string "false") <|> P.string "f"))

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

-- XXX: Dummy parser, add the real thing later.
number :: Parser Sexp
number = fmap (Number . read) $ many1 digit

-- Parse an S-Expression without lexing or delimiter handling
-- according to the tokens defined in the R⁷RS formal syntax:
--
--  <token> → <identifier> | <boolean> | <number>
--      | <character> | <string>
--      | ( | ) | #( | #u8( | ’ | ` | , | ,@ | .
--      <delimiter> → <whitespace> | <vertical line>
--
sexp' :: Parser Sexp
sexp' = identifier
        <|> character
        <|> number
        <|> string
        <|> (between (char '(') (char ')') list)
        -- Boolean, comments, and {bit,}vectors all start with
        -- a `#` character and thus require backtracking.
        <|> try boolean
        <|> try comment
        -- XXX: Treat vector and bytevector as list for now
        <|> try (between (P.string "#(") (P.char ')') list)
        <|> between (P.string "#u8(") (P.char ')') list
        -- XXX: Quotation tokens are ignored for now
        <|> (char '\'' >> sexp)
        <|> (char '`'  >> sexp)
        <|> ((P.string ",@" <|> P.string ",") >> sexp)
        -- TODO: Dotted pairs and dotted lists
        -- TODO: Directive (#!fold-case, …)
    where
        list :: Parser Sexp
        list = fmap List $ many sexp

-- Parse an s-expression with lexing and delimiter checking.
sexp :: Parser Sexp
sexp = terminatedBy (lexeme sexp') (lookAhead (delim <|> eof))
    where
        lexeme :: Parser a -> Parser a
        lexeme p = spaces >> p

        -- “Comments are treated exactly like whitespace.”
        delim :: Parser ()
        delim = (delimiter <|> (skip $ P.string "#|") <|> eof) <?> "delimiter"

-- Parse an R⁷RS Scheme program.
scheme :: Parser [Sexp]
scheme = manyTill sexp (try $ spaces >> eof)
-- TODO: Restructure to remove backtracking in manyTill.
-- This is needed because in the case of trailing whitespaces
-- the sexp parser will see the space and assume that a valid
-- token follows is since lexeme removes spaces at the start
-- and not at the end. Instead, spaces should be removed at
-- the end by lexeme though this interferes with delimiter.
