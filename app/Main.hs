module Main where

import System.Exit
import System.IO
import System.Environment
import Text.Parsec.String

import SchemeDoc
import SchemeDoc.Types
import SchemeDoc.Documentation.Markdown
import SchemeDoc.Parser.R7RS

-- TODO: Reuse include parser
parse :: Parser a -> String -> IO a
parse p fileName = parseFromFile p fileName >>= either report return
  where
    report err = do
        hPutStrLn stderr $ "Error: " ++ show err
        exitFailure

writeDoc :: DocLib -> IO ()
writeDoc lib = do
    decls <- docDecls lib
    putStrLn $ mkMarkdown (docFmt lib decls)

findDocLibs' :: [Sexp] -> IO ([DocLib])
findDocLibs' exprs =
    case findDocLibs exprs of
        Right libs -> pure libs
        Left err -> do
            hPutStrLn stderr $ "Error: " ++ show err
            exitFailure

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1
        then do
            hPutStrLn stderr "USAGE: scmdoc FILE"
            exitFailure
        else do
            source <- parse scheme $ head args
            libs <- findDocLibs' source
            _ <- mapM writeDoc libs
            pure ()
