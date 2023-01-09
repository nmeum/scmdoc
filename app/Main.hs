{-# LANGUAGE LambdaCase #-}
module Main where

import System.Exit
import System.IO
import System.Environment
import System.FilePath
import System.Directory
import Control.Exception

import SchemeDoc
import SchemeDoc.Error
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
        Left err -> throwIO $ ErrSyntax err

main' :: String -> IO ()
main' fileName =
  catch
   ( do
     source <- parseFromFile scheme fileName

     -- Expand all includes relative to given Scheme file.
     setCurrentDirectory $ takeDirectory fileName

     libs <- findDocLibs' source
     if null libs
         then hPutStrLn stderr "Warning: Found no documented define-library expression"
         else mapM writeDoc libs >> pure ()
   )
   ( \case
     ErrSyntax (SyntaxError expr err) ->
        hPutStrLn stderr $ "Syntax error on expression `" ++ (show expr) ++ "`: " ++ err
     ErrParser err ->
        hPutStrLn stderr $ show err
   )

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1
        then do
            hPutStrLn stderr "USAGE: scmdoc FILE"
            exitFailure
        else main' $ head args
