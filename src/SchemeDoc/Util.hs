module SchemeDoc.Util where

import Data.Char (isSpace)
import Control.Exception
import qualified Data.Text as T
import qualified Text.ParserCombinators.Parsec as P

import SchemeDoc.Error

-- Like Parsec's parseFromFile but throws an exception on error.
parseFromFile :: P.Parser a -> FilePath -> IO a
parseFromFile p fileName = do
    r <- P.parseFromFile p fileName
    case r of
        Left err -> throwIO $ ErrParser err
        Right s  -> pure s

-- Trim whitespaces from the left hand side.
ltrim :: T.Text -> T.Text
ltrim = T.dropWhile isSpace

-- Trim whitespaces from the right hand side.
rtrim :: T.Text -> T.Text
rtrim = T.dropWhileEnd isSpace

-- Trim all whitespaces from the left and right side of a Text.
trim :: T.Text -> T.Text
trim = rtrim . ltrim
