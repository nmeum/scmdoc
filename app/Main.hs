{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Exception
import Control.Monad
import qualified Data.Text as T
import Options.Applicative
import System.Directory
import System.FilePath
import System.IO

import SchemeDoc
import SchemeDoc.Error
import qualified SchemeDoc.Format.Library as L
import SchemeDoc.Parser.R7RS
import SchemeDoc.Types

data Opts = Opts
    { css :: String
    , title :: String
    , directory :: FilePath
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
       <> value "." )
    <*> argument str (metavar "FILE")
{- FOURMOLU_ENABLE -}

------------------------------------------------------------------------

libFileName :: DocLib -> FilePath
libFileName (_, l) =
    T.unpack $
        T.append
            (T.map (\c -> if c == ' ' then '-' else c) $ L.name l)
            (T.pack ".html")

writeDoc :: Opts -> FilePath -> DocLib -> IO ()
writeDoc (Opts optCss optTitle _ optFile) outFp docLib@(_, lib) = do
    -- Expand all includes relative to given Scheme file.
    (comps, failed) <- withCurrentDirectory (takeDirectory optFile) (docDecls docLib)

    forM_
        failed
        (\f -> warn $ "Failed to find formatter for:\n\n\t" ++ show f ++ "\n")
    forM_
        (findUndocumented lib comps)
        (\i -> warn $ "Exported but undocumented: " ++ show i)

    let hTitle =
            if null optTitle
                then show $ L.ident lib
                else optTitle

    let hbody = docFmt docLib comps
    let html = mkDoc hTitle optCss hbody

    writeFile outFp $ html ++ "\n"
  where
    warn msg = hPutStrLn stderr $ "WARNING: " ++ msg

findDocLibs' :: [Sexp] -> IO [DocLib]
findDocLibs' exprs =
    case findDocLibs exprs of
        Right libs -> pure libs
        Left err -> throwIO $ ErrSyntax err

main' :: Opts -> IO ()
main' opts@(Opts{library = optFile, directory = optDir}) =
    catch
        ( do
            srcs <- parseFromFile optFile
            libs <- findDocLibs' srcs

            -- Create output directory and its parents (mkdir -p).
            createDirectoryIfMissing True optDir

            if null libs
                then hPutStrLn stderr "Warning: Found no documented define-library expression"
                else
                    mapM_
                        (\l -> writeDoc opts (joinPath [optDir, libFileName l]) l)
                        libs
        )
        ( \case
            ErrSyntax (SyntaxError expr err) ->
                hPutStrLn stderr $ "Syntax error on expression `" ++ show expr ++ "`: " ++ err
            ErrParser err ->
                hPrint stderr err
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
