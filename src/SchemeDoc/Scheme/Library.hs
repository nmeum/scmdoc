module SchemeDoc.Scheme.Library
    (Library, findLibrary, mkExport, ExportSpec, expand, expand')
where

import Data.List (intercalate)
import Text.ParserCombinators.Parsec (parseFromFile)
import SchemeDoc
import SchemeDoc.Parser.R7RS

newtype LibraryName = MkLibName [String]
instance Show LibraryName where
    show (MkLibName lst) = intercalate " " lst

mkLibName :: Sexp -> Either SyntaxError LibraryName
mkLibName (List exprs) = MkLibName <$>
    mapM (\x -> case x of
        Id  ident -> Right $ ident
        Number n  -> Right $ (show n :: String)
        e         -> makeErr e "expected identifier or uinteger") exprs
mkLibName e = makeErr e "expected non-empty list"

------------------------------------------------------------------------

-- Export specification with internal name and external name.
-- For exports which are not renamed both are the same.
data ExportSpec = MkExport { internal :: String, external :: String }
    deriving (Show)

-- Export declaration for a library declaration.
--
--  <export spec> → <identifier>
--      | (rename <identifier> <identifier>)
--
exportDecl :: Sexp -> Either SyntaxError ExportSpec
exportDecl (List [Id "rename", Id i1, Id i2]) = Right $ MkExport i1 i2
exportDecl (Id i) = Right $ MkExport i i
exportDecl e = makeErr e "expected identifier or rename spec"

-- Export expression as part of a library declaration.
--
--   <export expr> → (export <export spec>*)
--
mkExport :: Sexp -> Either SyntaxError [ExportSpec]
mkExport (List ((Id "export"):items)) = foldr (\x acc -> case exportDecl x of
        Right e -> fmap ((:) e) acc
        Left e  -> Left e) (Right []) items
mkExport e = makeErr e "expected export list"

-- Find an export expression within a library declaration.
-- May return an empty list if no export declaration was found.
findExport :: [Sexp] -> Either SyntaxError [ExportSpec]
findExport (e@(List ((Id "export"):_)):_) = mkExport e
findExport (_:exprs) = findExport exprs
findExport [] = Right []

------------------------------------------------------------------------

-- An R⁷RS Scheme library as defined in Section 5.6 of the standard.
data Library = MkLibrary { name :: LibraryName
                         , exports :: [ExportSpec]
                         , body :: [Sexp] }
    deriving (Show)

-- Check if the given s-expression constitutes an R⁷RS library declaration.
findLibrary' :: Sexp -> Either SyntaxError Library
findLibrary' (List ((Id "define-library"):libraryName:xs)) = do
    libraryName' <- case mkLibName libraryName of
        Right n -> pure n
        Left err -> Left err

    exportSpec <- case findExport xs of
        Right e -> pure e
        Left err -> Left err

    pure $ MkLibrary libraryName' exportSpec xs
-- TODO: Handling of cond-expand?!
findLibrary' e = makeErr e "found no library definition"

-- Find library declarations in a Scheme source.
-- XXX: This function only considers top-level declarations.
findLibrary :: [Sexp] -> Either SyntaxError [Library]
findLibrary = foldr (\x acc -> case findLibrary' x of
                        Right lib -> fmap ((:) lib) acc
                        Left err  -> Left err) (Right [])

includeFile :: String -> IO [Sexp]
includeFile fileName = do
    putStrLn $ "loading: " ++ fileName
    r <- parseFromFile scheme fileName
    case r of
        Left err -> error $ show err
        Right s -> pure s

-- Expand an include into a begin expression.
--
--  <includer> →
--      | (include <string>+)
--      | (include-ci <string>+)
--
expand' :: Sexp -> IO Sexp
--expand' (List [(Id "include-ci"), fileNames]) = _
expand' (List ((Id "include"):fileNames)) = do
    begins <- (mapM includeFile paths) :: IO [[Sexp]]
    pure $ List ([Id "begin"] ++ concat begins)
    where
        paths :: [String]
        paths = map (\x -> case x of
            Str s -> s
            _     -> error "invalid file name") fileNames
expand' _ = error "not an include expression"

-- Expand the library declaration.
-- Returns all begin blocks, including any includes.
expand :: Library -> IO [Sexp]
expand (MkLibrary{body=decl}) = mapM expand' $ filter matches decl
    where
        matches :: Sexp -> Bool
        matches (List [(Id "begin"), _]) = True
        matches (List [(Id "include"), _]) = True
        matches (List [(Id "include-ci"), _]) = True
        matches _ = False
