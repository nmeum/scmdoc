{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Exception
import Control.Monad
import Options.Applicative
import System.Directory
import System.FilePath
import System.IO

import SchemeDoc
import SchemeDoc.Error
import SchemeDoc.Format.Library
import SchemeDoc.Parser.R7RS
import SchemeDoc.Types

data Opts = Opts
    { css :: String
    , title :: String
    , output :: FilePath
    , library :: FilePath
    }

{- FOURMOLU_DISABLE -}
parseOpts :: Parser Opts
parseOpts = Opts
    <$> option str
        ( long "stylesheet"
       <> short 's'
       <> value "https://cdn.jsdelivr.net/gh/kognise/water.css@latest/dist/dark.css" )
    <*> option str
        ( long "title"
       <> short 't'
       <> value "" )
    <*> option str
        ( long "output"
       <> short 'o'
       <> value "-" )
    <*> argument str (metavar "FILE")
{- FOURMOLU_ENABLE -}

------------------------------------------------------------------------

writeDoc :: Opts -> DocLib -> IO ()
writeDoc (Opts optCss optTitle optOut _) docLib@(_, lib@Library{ident = n}) = do
    (comps, failed) <- docDecls docLib

    forM_
        failed
        (\f -> warn $ "Failed to find formatter for:\n\n\t" ++ (show f) ++ "\n")
    forM_
        (findUndocumented lib comps)
        (\i -> warn $ "Exported but undocumented: " ++ (show i))

    let hTitle =
            if null optTitle
                then show $ n
                else optTitle

    let hbody = docFmt docLib comps
    let html = mkDoc hTitle optCss hbody
    if optOut == "-"
        then putStrLn html
        else writeFile optOut $ html ++ "\n"
  where
    warn msg = hPutStrLn stderr $ "WARNING: " ++ msg

findDocLibs' :: [Sexp] -> IO ([DocLib])
findDocLibs' exprs =
    case findDocLibs exprs of
        Right libs -> pure libs
        Left err -> throwIO $ ErrSyntax err

main' :: Opts -> IO ()
main' opts@(Opts{library = optFile}) =
    catch
        ( do
            source <- parseFromFile optFile

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
    opts =
        info
            (parseOpts <**> helper)
            ( fullDesc
                <> progDesc "Generate HTML documentation for a R⁷RS Scheme library"
            )
