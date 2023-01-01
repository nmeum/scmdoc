{-# LANGUAGE LambdaCase #-}
module SchemeDoc.Scheme.Includer (expand) where

import SchemeDoc
import SchemeDoc.Parser.R7RS

import Control.Exception
import qualified Text.ParserCombinators.Parsec as P

data ExpandException = ErrSyntax SyntaxError | ErrParser P.ParseError
    deriving Show

instance Exception ExpandException

-- Helper function to throw a syntax error.
throwSyntax :: Sexp -> String -> IO a
throwSyntax e m = throwIO $ ErrSyntax (SyntaxError e m)

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
expand :: Sexp -> IO Sexp
-- TODO: Implementing include-ci requires toLower form Data.Text
expand (List ((Id "include-ci"):_)) = error "include-ci not implemented"
expand (List ((Id "include"):fileNames)) = do
    paths <- mapM (\case
                    Str s -> pure s
                    e     -> throwSyntax e "expected list of strings")
             fileNames

    exprs <- (mapM (parseFromFile scheme) paths)
    pure $ List ([Id "begin"] ++ concat exprs)
expand e = throwSyntax e "not an include expression"
