module Parser.Util where

import Numeric (readHex)
import Text.ParserCombinators.Parsec

-- Parse a hexadecimal number without a prefix.
hex :: Parser Int
hex = fmap readHex' hexDigits
    where
        -- Due to hexDigits, readHex should always read the full string.
        -- XXX: Technically R⁷RS does not allow upper case hex letters.
        readHex'  = fst . head . readHex
        hexDigits = many1 (oneOf "0123456789abcdefABCDEF")

-- Bind a given character to the given result.
bind :: String -> a -> Parser a
bind str val = const val <$> string str

------------------------------------------------------------------------

filterJust :: [Maybe a] -> [a]
filterJust = foldr (\x acc -> case x of
                                Nothing -> acc
                                Just x' -> x' : acc) []
