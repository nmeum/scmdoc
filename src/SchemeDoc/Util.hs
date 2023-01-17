module SchemeDoc.Util where

import Control.Exception
import qualified Text.ParserCombinators.Parsec as P

import SchemeDoc.Error

-- Like Parsec's parseFromFile but throws an exception on error.
parseFromFile :: P.Parser a -> FilePath -> IO a
parseFromFile p fileName = do
    r <- P.parseFromFile p fileName
    case r of
        Left err -> throwIO $ ErrParser err
        Right s  -> pure s
