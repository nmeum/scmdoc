module SchemeDoc.Scheme.Library
    (Library, findLibraries, libName, libExports, libExpand)
where

import Control.Monad (foldM)
import Data.List (intercalate)
import SchemeDoc
import SchemeDoc.Scheme.Includer

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
data Library = MkLibrary { name    :: LibraryName
                         , exports :: [ExportSpec] -- TODO: Make this a set
                         , body    :: [Sexp] }
    deriving (Show)

-- Check if the given s-expression constitutes an R⁷RS library declaration.
findLibraries' :: Sexp -> Either SyntaxError Library
findLibraries' (List ((Id "define-library"):libraryName:xs)) = do
    libraryName' <- case mkLibName libraryName of
        Right n  -> pure n
        Left err -> Left err

    exportSpec <- case findExport xs of
        Right e  -> pure e
        Left err -> Left err

    pure $ MkLibrary libraryName' exportSpec xs
-- TODO: Handling of cond-expand?!
findLibraries' e = makeErr e "found no library definition"

-- Find library declarations in a Scheme source.
-- XXX: This function only considers top-level declarations.
findLibraries :: [Sexp] -> Either SyntaxError [Library]
findLibraries = foldr (\x acc -> case findLibraries' x of
                        Right lib -> fmap ((:) lib) acc
                        Left err  -> Left err) (Right [])

-- Name of the library.
-- Multiple identifiers are joined by a single ' ' character.
libName :: Library -> String
libName = show . name

-- Whether the library exports the given **internal** identifier.
libExports :: Library -> String -> Bool
libExports lib ident = any (\MkExport{internal=i} -> i == ident) $
                           exports lib

-- Expand the library declaration.
-- Returns all begin blocks, including any include expressions.
libExpand :: Library -> IO [Sexp]
libExpand (MkLibrary{body=decl}) = foldM libExpand' [] decl
    where
        libExpand' :: [Sexp] -> Sexp -> IO [Sexp]
        libExpand' acc e@(List ((Id "begin"):_))      = pure $ e : acc
        libExpand' acc e@(List ((Id "include"):_))    = expand e >>= (\x -> pure $ x : acc)
        libExpand' acc e@(List ((Id "include-ci"):_)) = expand e >>= (\x -> pure $ x : acc)
        libExpand' acc _ = pure acc
