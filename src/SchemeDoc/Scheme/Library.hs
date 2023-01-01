module SchemeDoc.Scheme.Library
    (Library, findLibrary, mkExport, ExportSpec)
where

import Data.List (intercalate)
import SchemeDoc

newtype LibraryName = MkLibName [String]
instance Show LibraryName where
    show (MkLibName lst) = intercalate " " lst

mkLibName :: Sexp -> Either SyntaxError LibraryName
mkLibName (List exprs) = MkLibName <$>
    foldr (\x acc -> case x of
        Id  ident -> ((:) ident) <$> acc
        Number n  -> ((:) (show n :: String)) <$> acc
        e         -> makeErr e "expected identifier or uinteger") (Right []) exprs
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
