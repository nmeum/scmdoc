module Main where

import System.Exit
import System.IO
import System.Environment

import SchemeDoc
import SchemeDoc.Types
import SchemeDoc.Parser.R7RS
import SchemeDoc.Output
import SchemeDoc.Util (parseFromFile)

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

main' :: String -> IO ()
main' fileName = do
    source <- parseFromFile scheme fileName
    libs <- findDocLibs' source
    _ <- mapM writeDoc libs
    pure ()

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1
        then do
            hPutStrLn stderr "USAGE: scmdoc FILE"
            exitFailure
        else main' $ head args
