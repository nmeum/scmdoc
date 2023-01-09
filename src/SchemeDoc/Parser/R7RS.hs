module SchemeDoc.Parser.R7RS (scheme) where

import SchemeDoc.Types
import SchemeDoc.Parser.Util
import SchemeDoc.Parser.Number

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
mnemonicEsc = char '\\' >>
          (
               bind "a" '\a' -- alarm
           <|> bind "b" '\b' -- backspace
           <|> bind "t" '\t' -- character tabulation
           <|> bind "n" '\n' -- linefeed
           <|> bind "r" '\r' -- return
          )

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

-- Source code comment.
--
--  <comment> → ; <all subsequent characters up to a line ending>
--      | <nested comment>
--      | #; <intertoken space> <datum>
--
-- This parser intentionally ignores documentation comments.
-- See the docComment parser below.
comment :: Parser ()
comment = skip $
          (char ';' >> notFollowedBy (P.string ";>") >> manyTill anyChar endOfLine)
           <|> nestedComment
      -- TODO: #; comments

------------------------------------------------------------------------

-- A source code comment for documentation purposes.
-- This is a custom SchemeDoc specific grammar rule.
--
--  <doc comment> → ;;> <all subsequent characters up to a line ending>
--
docComment :: Parser Sexp
docComment = fmap (DocComment . ltrim . concat) $
    (many1 $ P.string ";;>" >> manyTill anyChar endOfLine)

-- Sign subsequent for peculiar identifier.
--
--  <sign subsequent> → <initial> | <explicit sign> | @
--
signSubsequent :: Parser String
signSubsequent = do
    first <- initial
    sign  <- explicitSign
    ch    <- char '@'
    return [first, sign, ch]

-- Dot subsequent for peculiar identifier.
--
--   | . <dot subsequent> <subsequent>*
--
dotSubsequent :: Parser String
dotSubsequent = do
    first <- char '.'
    subsq <- many subsequent
    return (first : subsq)

-- Peculiar identifier.
--
--  <peculiar identifier> → <explicit sign>
--      | <explicit sign> <sign subsequent> <subsequent>*
--      | <explicit sign> . <dot subsequent> <subsequent>*
--      | . <dot subsequent> <subsequent>*
--
peculiarIdentifier :: Parser String
peculiarIdentifier = fmap (\x -> [x]) (try explicitSign)
                 <|> try
                  (do
                    sign <- explicitSign
                    ssub <- signSubsequent
                    sub  <- many subsequent
                    return ([sign] ++ ssub ++ sub)
                  )
                 <|> try
                  (do
                    sign <- explicitSign
                    dsub <- dotSubsequent
                    sub  <- many subsequent
                    return ([sign] ++ dsub ++ sub)
                  )
                 <|> try
                  (do
                    dot  <- char '.'
                    dsub <- dotSubsequent
                    sub  <- many subsequent
                    return ([dot] ++ dsub ++ sub)
                  )

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
        <|> peculiarIdentifier

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

-- Parse a list, e.g. (1 2 3).
list :: Parser Sexp
list = fmap List $ char '(' >> (manyTill sexp (try $ lexeme $ char ')'))

-- Parse syntatic sugar for vectors, e.g. `#(1 2 3)`.
vector :: Parser Sexp
vector = fmap (\lst -> List $ Id "vector" : lst) $
    P.string "#(" >> (manyTill sexp (try $ lexeme $ char ')'))

-- Parse syntatic sugar for bytevectors, e.g. `#u8(1 2 3)`.
bytevector :: Parser Sexp
bytevector = fmap (\lst -> List $ Id "bytevector" : lst) $
    P.string "#u8(" >> (manyTill sexp (try $ lexeme $ char ')'))

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
        <|> list
        <|> docComment
        -- Boolean, comments, and {bit,}vectors all start with
        -- a `#` character and thus require backtracking.
        <|> try boolean
        <|> try vector
        <|> bytevector
        -- XXX: Quotation tokens are ignored for now
        <|> (char '\'' >> sexp)
        <|> (char '`'  >> sexp)
        <|> ((P.string ",@" <|> P.string ",") >> sexp)
        -- TODO: Dotted pairs and dotted lists
        -- TODO: Directive (#!fold-case, …)

-- Parse an s-expression with lexing and delimiter checking.
sexp :: Parser Sexp
sexp = terminatedBy (lexeme sexp') (lookAhead (delim <|> eof))
    where
        delim :: Parser ()
        delim = (delimiter <|> (skip $ P.string "#|") <|> eof) <?> "delimiter"

-- Parse an R⁷RS Scheme program.
scheme :: Parser [Sexp]
scheme = manyTill sexp (try $ lexeme eof)
-- TODO: Restructure to remove backtracking in manyTill.
-- This is needed because in the case of trailing whitespaces
-- the sexp parser will see the space and assume that a valid
-- token follows is since lexeme removes spaces at the start
-- and not at the end. Instead, spaces should be removed at
-- the end by lexeme though this interferes with delimiter.

-- “Comments are treated exactly like whitespace.”
-- Comments require backtracking for docComment parser.
lexComment :: Parser a -> Parser a
lexComment p = lexComment' >> p
  where
    lexComment' = skipMany ((try comment) >> spaces)

-- Strip whitespaces and comments between tokens.
lexeme :: Parser a -> Parser a
lexeme p = spaces >> lexComment p
