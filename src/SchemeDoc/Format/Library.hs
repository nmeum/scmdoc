{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module SchemeDoc.Format.Library
    (Library(..), mkLibrary, libName, libExports, libExpand, ExportSpec(..))
where

import Control.Monad (foldM)

import SchemeDoc.Types
import SchemeDoc.Util
import SchemeDoc.Error
import SchemeDoc.Format.Types
import SchemeDoc.Format.Util
import SchemeDoc.Parser.R7RS

import Text.Blaze.Html
import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H

-- An R⁷RS Scheme library as defined in Section 5.6 of the standard.
data Library = Library { libIdent  :: LibraryName
                       , libExport :: [ExportSpec] -- TODO: Make this a set
                       , libBody   :: [Sexp] }
    deriving (Show)

instance Formatable Library where
    fmt lib desc = Declaration (libName lib) desc $ do
                        H.h1 $ toHtml (libName lib)
                        fromMkd desc

-- Parse a Scheme library definition.
--
--  <library> → (define-library <library name>
--      <library declaration>*)
--
-- where
--
--  <library name part> → <identifier> | <uinteger 10>
--      <library declaration> → (export <export spec>*)
--      | <import declaration>
--      | (begin <command or definition>*)
--      | <includer>
--      | (include-library-declarations <string>+)
--      | (cond-expand <cond-expand clause>+)
--      | (cond-expand <cond-expand clause>+
--          (else <library declaration>*))
--
mkLibrary :: Sexp -> Either SyntaxError Library
mkLibrary (List ((Id "define-library"):libraryName:xs)) = do
    libraryName' <- case mkLibName libraryName of
        Right n  -> pure n
        Left err -> Left err

    exportSpec <- case findExport xs of
        Right e  -> pure e
        Left err -> Left err

    pure $ Library libraryName' exportSpec xs
-- TODO: Handling of cond-expand?!
mkLibrary e = makeErr e "found no library definition"

-- Name of the library.
-- Multiple identifiers are joined by a single ' ' character.
libName :: Library -> T.Text
libName (Library{libIdent=n}) = libName' n

-- Whether the library exports the given **internal** identifier.
libExports :: Library -> T.Text -> Bool
libExports lib ident = any (\Export{internal=i} -> i == ident) $
                            libExport lib

-- Expand an include into a begin expression.
--
--  <includer> →
--      | (include <string>+)
--      | (include-ci <string>+)
--
expand :: Sexp -> IO Sexp
expand (List ((Id "include-ci"):fileNames)) = expand' fileNames True
expand (List ((Id "include"):fileNames)) = expand' fileNames False
expand e = throwSyntax e "not an include expression"

expand' :: [Sexp] -> Bool -> IO Sexp
expand' fileNames lower = do
    paths <- mapM (\case
                    Str s -> pure $ if lower then T.toLower s else s
                    e     -> throwSyntax e "expected list of strings")
             fileNames

    exprs <- (mapM (parseFromFile scheme . T.unpack) paths)
    pure $ List ([Id "begin"] ++ concat exprs)

-- Expand the library declaration.
-- Returns all begin blocks, including includer expressions as expanded begin blocks.
libExpand :: Library -> IO [Sexp]
libExpand (Library{libBody=decl}) = foldM libExpand' [] decl
    where
        libExpand' :: [Sexp] -> Sexp -> IO [Sexp]
        libExpand' acc e@(List ((Id "begin"):_))      = pure $ e : acc
        libExpand' acc e@(List ((Id "include"):_))    = expand e >>= (\x -> pure $ x : acc)
        libExpand' acc e@(List ((Id "include-ci"):_)) = expand e >>= (\x -> pure $ x : acc)
        libExpand' acc _ = pure acc

------------------------------------------------------------------------

newtype LibraryName = LibName [T.Text]
instance Show LibraryName where
    show = T.unpack . libName'

libName' :: LibraryName -> T.Text
libName' (LibName lst) = T.intercalate " " lst

-- Parses a Scheme library name.
--
--  <library name part> → <identifier> | <uinteger 10>
--
mkLibName :: Sexp -> Either SyntaxError LibraryName
mkLibName (List exprs) = LibName <$>
    mapM (\x -> case x of
        Id  ident -> Right $ ident
        -- TODO: Only allow <uinteger 10> in library name
        Number n  -> Right $ T.pack (show n :: String)
        e         -> makeErr e "expected identifier or uinteger") exprs
mkLibName e = makeErr e "expected non-empty list"

------------------------------------------------------------------------

-- Export specification with internal name and external name.
-- For exports which are not renamed both are the same.
data ExportSpec = Export { internal :: T.Text, external :: T.Text }
    deriving (Show)

-- Export declaration for a library declaration.
--
--  <export spec> → <identifier>
--      | (rename <identifier> <identifier>)
--
exportDecl :: Sexp -> Either SyntaxError ExportSpec
exportDecl (List [Id "rename", Id i1, Id i2]) = Right $ Export i1 i2
exportDecl (Id i) = Right $ Export i i
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
