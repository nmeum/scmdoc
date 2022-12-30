module Main where

import System.Exit
import System.IO
import System.Environment

import Parser.Scheme
import Text.Parsec.String

parse :: Parser a -> String -> IO a
parse p fileName = parseFromFile p fileName >>= either report return
  where
    report err = do
        hPutStrLn stderr $ "Error: " ++ show err
        exitFailure

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1
        then error "invalid amount of args"
        else do
            r <- parse scheme (head args)
            putStrLn $ show r
