module MyLib where

import Text.ParserCombinators.Parsec

data Sexp = Atom String
          | List [Sexp]
    deriving (Show)

spaces1 :: Parser ()
spaces1 = skipMany1 space

parseList :: Parser Sexp
parseList = fmap List $ sepBy parseSexp spaces1

parseAtom :: Parser Sexp
parseAtom = fmap Atom $ many1 alphaNum

parseSexp :: Parser Sexp
parseSexp = parseAtom
        <|> between (char '(') (char ')') parseList

------------------------------------------------------------------------

someFunc :: IO ()
someFunc = putStrLn "someFunc"
