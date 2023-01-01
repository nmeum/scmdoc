{-# LANGUAGE LambdaCase #-}
module SchemeDoc.Scheme.Includer (expand) where

import SchemeDoc
import SchemeDoc.Parser.R7RS

import Data.Char (toLower)
import Control.Exception
import qualified Text.ParserCombinators.Parsec as P

data ExpandException = ErrSyntax SyntaxError | ErrParser P.ParseError
    deriving Show

instance Exception ExpandException

-- Helper function to throw a syntax error.
throwSyntax :: Sexp -> String -> IO a
throwSyntax e m = throwIO $ ErrSyntax (SyntaxError e m)

-- The Scheme string-foldcase procedure.
foldcase :: String -> String
foldcase = map toLower

------------------------------------------------------------------------

-- Like Parsec's parseFromFile but throws an exception on error.
parseFromFile :: P.Parser a -> String -> IO a
parseFromFile p fileName = do
    r <- P.parseFromFile p fileName
    case r of
        Left err -> throwIO $ ErrParser err
        Right s  -> pure s

-- Expand an include into a begin expression.
--
--  <includer> →
--      | (include <string>+)
--      | (include-ci <string>+)
--
expand' :: [Sexp] -> Bool -> IO Sexp
expand' fileNames lower = do
    paths <- mapM (\case
                    Str s -> pure $ if lower then foldcase s else s
                    e     -> throwSyntax e "expected list of strings")
             fileNames

    exprs <- (mapM (parseFromFile scheme) paths)
    pure $ List ([Id "begin"] ++ concat exprs)

expand :: Sexp -> IO Sexp
expand (List ((Id "include-ci"):fileNames)) = expand' fileNames True
expand (List ((Id "include"):fileNames)) = expand' fileNames False
expand e = throwSyntax e "not an include expression"
