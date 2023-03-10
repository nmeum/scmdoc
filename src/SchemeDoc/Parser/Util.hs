module SchemeDoc.Parser.Util where

import Control.Monad (void)
import Numeric (readHex)
import Text.ParserCombinators.Parsec

-- Parse a hexadecimal number without a prefix.
hex :: Parser Int
hex = fmap readHex' hexDigits
  where
    -- Due to hexDigits, readHex should always read the full string.
    -- XXX: Technically R⁷RS does not allow upper case hex letters.
    readHex' = fst . head . readHex
    hexDigits = many1 (oneOf "0123456789abcdefABCDEF")

-- Bind a given character to the given result.
bind :: String -> a -> Parser a
bind str val = val <$ string str

-- Like skipMany but without the many part.
skip :: Parser a -> Parser ()
skip = void

-- Runs both parsers and returns the result of the first.
terminatedBy :: Parser a -> Parser b -> Parser a
terminatedBy p1 p2 = do
    r <- p1
    _ <- p2
    return r

-- Like manyTill but preserve the terminator in the return value.
manyTill' :: Parser a -> Parser a -> Parser [a]
manyTill' p end = scan
  where
    scan =
        do x <- end; return [x]
        <|> do x <- p; xs <- scan; return (x : xs)

------------------------------------------------------------------------

-- Remove all Nothing value from a List of Maybe monads.
filterJust :: [Maybe a] -> [a]
filterJust =
    foldr
        ( \x acc -> case x of
            Nothing -> acc
            Just x' -> x' : acc
        )
        []
