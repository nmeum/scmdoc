{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Exception
import Control.Monad
import qualified Data.Text as T
import Options.Applicative
import System.Directory
import System.FilePath
import System.IO
import System.Exit (die)

import SchemeDoc
import SchemeDoc.Error
import qualified SchemeDoc.Format.Library as L
import SchemeDoc.Parser.R7RS
import SchemeDoc.Types

data Opts = Opts
    { css :: String
    , title :: String
    , directory :: FilePath
    , libraries :: [FilePath]
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
    <*> some (argument str (metavar "FILE..."))
{- FOURMOLU_ENABLE -}

------------------------------------------------------------------------

libFileName :: DocLib -> FilePath
libFileName (_, l) =
    T.unpack $
        T.append
            (T.map (\c -> if c == ' ' then '/' else c) $ L.name l)
            (T.pack ".html")

writeDoc :: Opts -> FilePath -> FilePath -> DocLib -> IO ()
writeDoc (Opts optCss optTitle _ _) inFp outFp docLib@(_, lib) = do
    -- Expand all includes relative to given Scheme file.
    (comps, failed) <- withCurrentDirectory (takeDirectory inFp) (docDecls docLib)

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

writeAll :: Opts -> [(FilePath, DocLib)] -> IO ()
writeAll opts@(Opts{directory = optDir}) =
    mapM_
        (\(p, l) -> mkDestPath l >>= flip (writeDoc opts p) l)
  where
    mkDestPath :: DocLib -> IO FilePath
    mkDestPath l =
        let fp = joinPath [optDir, libFileName l]
         in fp <$ createDirectoryIfMissing True (takeDirectory fp)

findAllLibs :: [[Sexp]] -> [FilePath] -> IO [(FilePath, DocLib)]
findAllLibs srcs files =
    foldM
        ( \acc (path, src) -> do
            l <- findDocLibs' src
            pure $ map ((,) path) l ++ acc
        )
        []
        (zip files srcs)
  where
    findDocLibs' :: [Sexp] -> IO [DocLib]
    findDocLibs' exprs = either (throwIO . ErrSyntax) pure $ findDocLibs exprs

main' :: Opts -> IO ()
main' opts@(Opts{libraries = optFiles}) =
    catch
        ( do
            srcs <- mapM parseFromFile optFiles
            libs <- findAllLibs srcs optFiles

            if null libs
                then die "Found no documented define-library expression"
                else writeAll opts libs
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
