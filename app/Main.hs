module Main where

import System.Exit
import System.IO
import System.Environment

import SchemeDoc
import SchemeDoc.Formatter
import SchemeDoc.Documentation.Markdown
import SchemeDoc.Scheme.Library
import SchemeDoc.Scheme.Documented
import SchemeDoc.Parser.R7RS
import Text.Parsec.String

parse :: Parser a -> String -> IO a
parse p fileName = parseFromFile p fileName >>= either report return
  where
    report err = do
        hPutStrLn stderr $ "Error: " ++ show err
        exitFailure

getDoc :: Library -> IO String
getDoc lib = do
    decls <- libExpand lib
    let docs = findDocumented decls
    pure $ "# " ++ libName lib ++ "\n\n" ++ (mkMarkdown $ format docs)

getDocs :: [Sexp] -> IO String
getDocs src = do
    case findLibraries src of
        Right libs -> getDoc $ head libs
        Left err -> error (show err)

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1
        then error "invalid amount of args"
        else do
            r <- parse scheme (head args)
            doc <- getDocs r
            putStrLn doc
