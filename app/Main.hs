{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO
import System.FilePath
import System.Directory
import Control.Exception
import Control.Monad
import Options.Applicative

import SchemeDoc
import SchemeDoc.Error
import SchemeDoc.Types
import SchemeDoc.Parser.R7RS
import SchemeDoc.Format.Library
import SchemeDoc.Util (parseFromFile)

data Opts = Opts
    { css     :: String
    , title   :: String
    , output  :: FilePath
    , library :: FilePath }

parseOpts :: Parser Opts
parseOpts = Opts
    <$> option auto
        ( long "css"
       <> short 's'
       <> value "https://cdn.jsdelivr.net/gh/kognise/water.css@latest/dist/dark.css" )
    <*> option auto
        ( long "title"
       <> short 't'
       <> value "" )
    <*> option str
        ( long "output"
       <> short 'o'
       <> value "-" )
    <*> argument str (metavar "FILE")

------------------------------------------------------------------------

writeDoc :: Opts -> DocLib -> IO ()
writeDoc (Opts optCss optTitle optOut _) docLib@(_, Library{name=n}) = do
    decls <- docDecls docLib
    let hTitle = if null optTitle
                    then show $ n
                    else optTitle

    let (hbody, failed) = docFmt docLib decls
    forM_ failed (\f -> hPutStrLn stderr $
        "WARNING: Failed to find formatter for documented S-expression:\n\n\t" ++ show f ++ "\n")

    let html = mkDoc hTitle optCss hbody
    if optOut == "-"
        then putStrLn html
        else writeFile optOut $ html ++ "\n"

findDocLibs' :: [Sexp] -> IO ([DocLib])
findDocLibs' exprs =
    case findDocLibs exprs of
        Right libs -> pure libs
        Left err -> throwIO $ ErrSyntax err

main' :: Opts -> IO ()
main' opts@(Opts{library=optFile}) =
  catch
   ( do
     source <- parseFromFile scheme optFile

     -- Expand all includes relative to given Scheme file.
     setCurrentDirectory $ takeDirectory optFile

     libs <- findDocLibs' source
     if null libs
         then hPutStrLn stderr "Warning: Found no documented define-library expression"
         else mapM (writeDoc opts) libs >> pure ()
   )
   ( \case
     ErrSyntax (SyntaxError expr err) ->
        hPutStrLn stderr $ "Syntax error on expression `" ++ (show expr) ++ "`: " ++ err
     ErrParser err ->
        hPutStrLn stderr $ show err
   )

main :: IO ()
main = main' =<< execParser opts
  where
    opts = info (parseOpts <**> helper)
        ( fullDesc
       <> progDesc "Generate HTML documentation for a R⁷RS Scheme library" )
