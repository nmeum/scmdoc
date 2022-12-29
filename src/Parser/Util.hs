module Parser.Util (spaces1, hex) where

import Numeric (readHex)
import Text.ParserCombinators.Parsec

-- Like spaces from Parsec but matches one or more spaces.
spaces1 :: Parser ()
spaces1 = skipMany1 space

-- Parse a hexadecimal number without a prefix.
hex :: Parser Int
hex = fmap readHex' hexDigits
    where
        -- Due to hexDigits, readHex should always read the full string.
        readHex'  = fst . head . readHex
        hexDigits = many1 (oneOf "0123456789abcdefABCDEF")
