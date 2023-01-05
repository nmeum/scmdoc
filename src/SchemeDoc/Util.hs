module SchemeDoc.Util where

import Data.Char (toLower)
import Control.Exception
import qualified Text.ParserCombinators.Parsec as P

import SchemeDoc.Error

-- The Scheme string-foldcase procedure.
foldcase :: String -> String
foldcase = map toLower

-- Like Parsec's parseFromFile but throws an exception on error.
parseFromFile :: P.Parser a -> String -> IO a
parseFromFile p fileName = do
    r <- P.parseFromFile p fileName
    case r of
        Left err -> throwIO $ ErrParser err
        Right s  -> pure s
