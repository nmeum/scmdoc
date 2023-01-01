module SchemeDoc.Scheme.Library
    (Library, findLibrary, mkExport, ExportSpec)
where

import Data.List (intercalate)
import SchemeDoc

-- TODO: Custem error type for all Eithers
-- which also captures the failed S-Expression.

newtype LibraryName = MkLibName [String]
instance Show LibraryName where
    show (MkLibName lst) = intercalate " " lst

mkLibName :: Sexp -> Either String LibraryName
mkLibName (List exprs) = MkLibName <$>
    foldr (\x acc -> case x of
        Id  ident -> fmap ((:) ident) acc
        -- TODO: Only allow exact non-negative integers.
        Number n  -> fmap ((:) (show n :: String)) acc
        _         -> Left "expected identifier or uinteger") (Right []) exprs
mkLibName _ = Left "expected non-empty list"

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
exportDecl :: Sexp -> Either String ExportSpec
exportDecl (List [Id "rename", Id i1, Id i2]) = Right $ MkExport i1 i2
exportDecl (Id i) = Right $ MkExport i i
exportDecl _ = Left "expected identifier or rename spec"

-- Export expression as part of a library declaration.
--
--   <export expr> → (export <export spec>*)
--
mkExport :: Sexp -> Either String [ExportSpec]
mkExport (List ((Id "export"):exports)) = foldr (\x acc -> case exportDecl x of
        Right e -> fmap ((:) e) acc
        Left e  -> Left e) (Right []) exports
mkExport _ = Left "expected export list"

------------------------------------------------------------------------

-- An R⁷RS Scheme library as defined in Section 5.6 of the standard.
data Library = MkLibrary { name :: LibraryName
                         --, exports :: [String]
                         , body :: [Sexp] }
    deriving (Show)

-- Check if the given s-expression constitutes an R⁷RS library declaration.
findLibrary' :: Sexp -> Either String Library
findLibrary' (List ((Id "define-library"):x:xs)) =
    fmap (\n -> MkLibrary n xs) $ mkLibName x
findLibrary' _ = Left "found no library definition"

-- Find library declarations in a Scheme source.
-- XXX: This function only considers top-level declarations.
findLibrary :: [Sexp] -> Either String [Library]
findLibrary = foldr (\x acc -> case findLibrary' x of
                        Right lib -> fmap ((:) lib) acc
                        Left err  -> Left err) (Right [])
