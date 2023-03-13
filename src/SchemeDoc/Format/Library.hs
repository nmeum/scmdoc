{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module implements a 'Formatter' for Scheme library definitions.
--
-- For example:
--
-- > (define-library (math arithmetic)
-- >   (export my-proc)
-- >
-- >   (begin
-- >     (define (my-proc x)
-- >       (* x 2))))
module SchemeDoc.Format.Library (
    Library (..),
    mkLibrary,
    name,
    externalId,
    exports,
    expand,
    ExportSpec (..),
    LibraryName,
)
where

import Control.Monad (foldM)
import Data.List (find)

import SchemeDoc.Error
import SchemeDoc.Format.Types
import SchemeDoc.Format.Util
import SchemeDoc.Parser.R7RS
import SchemeDoc.Types

import qualified Data.Text as T
import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H

-- | An R7RS Scheme library as defined in Section 5.6 of the standard.
data Library = Library
    { ident :: LibraryName
    -- ^ Library name.
    , exported :: [ExportSpec]
    -- ^ Export declaration of the library.
    , decl :: [Sexp]
    -- ^ Declarations of the library.
    }
    deriving (Show)

instance Formatable Library where
    fmt lib desc = mkDeclaration (name lib) desc $ \n ->
        do
            H.h1 $ toHtml n
            fromMkd desc

-- | Parse a Scheme library definition.
--
-- > <library> → (define-library <library name>
-- >     <library declaration>*)
--
-- where
--
-- > <library name part> → <identifier> | <uinteger 10>
-- >     <library declaration> → (export <export spec>*)
-- >     | <import declaration>
-- >     | (begin <command or definition>*)
-- >     | <includer>
-- >     | (include-library-declarations <string>+)
-- >     | (cond-expand <cond-expand clause>+)
-- >     | (cond-expand <cond-expand clause>+
-- >         (else <library declaration>*))
mkLibrary :: Sexp -> Either SyntaxError Library
mkLibrary (List ((Id "define-library") : libraryName : xs)) = do
    libraryName' <- case mkLibName libraryName of
        Right n -> pure n
        Left err -> Left err

    exportSpec <- case findExport xs of
        Right e -> pure e
        Left err -> Left err

    pure $ Library libraryName' exportSpec xs
-- TODO: Handling of cond-expand?!
mkLibrary e = makeErr e "found no library definition"

-- | Name of the 'Library'. Multiple identifiers, within the library
-- name, are joined by a single @' '@ character.
name :: Library -> T.Text
name (Library{ident = n}) = name' n

-- | Return the external identifier for the given internal identifier.
externalId :: Library -> T.Text -> Maybe T.Text
externalId lib n =
    external
        <$> find (\e -> internal e == n) (exported lib)

-- | Whether the 'Library' exports the given **internal** identifier.
exports :: Library -> T.Text -> Bool
exports lib i =
    any (\Export{internal = i'} -> i == i') $
        exported lib

-- Expand an include into a begin expression.
--
--  <includer> →
--      | (include <string>+)
--      | (include-ci <string>+)
--
-- TODO: Support include-ci
expandIncl :: Sexp -> IO Sexp
expandIncl (List ((Id "include") : fileNames)) = expandIncl' fileNames
expandIncl e@(List ((Id "include-ci") : _)) = throwSyntax e "include-ci currently not supported"
expandIncl e = throwSyntax e "not an include expression"

expandIncl' :: [Sexp] -> IO Sexp
expandIncl' fileNames = do
    paths <-
        mapM
            ( \case
                Str s -> pure s
                e -> throwSyntax e "expected list of strings"
            )
            fileNames

    exprs <- mapM (parseFromFile . T.unpack) paths
    pure $ List (Id "begin" : concat exprs)

-- | Expand the library declaration. Returns all begin blocks, including
-- includer expressions as expanded begin blocks.
expand :: Library -> IO [Sexp]
expand (Library{decl = d}) = foldM expand' [] d
  where
    expand' :: [Sexp] -> Sexp -> IO [Sexp]
    expand' acc e@(List ((Id "begin") : _)) = pure $ e : acc
    expand' acc e@(List ((Id "include") : _)) = expandIncl e >>= (\x -> pure $ x : acc)
    expand' acc e@(List ((Id "include-ci") : _)) = expandIncl e >>= (\x -> pure $ x : acc)
    expand' acc _ = pure acc

------------------------------------------------------------------------

-- | Name of a R7RS Scheme library.
newtype LibraryName = LibName [T.Text]

instance Show LibraryName where
    show = T.unpack . name'

name' :: LibraryName -> T.Text
name' (LibName lst) = T.intercalate " " lst

-- Parses a Scheme library name.
--
--  <library name part> → <identifier> | <uinteger 10>
--
mkLibName :: Sexp -> Either SyntaxError LibraryName
mkLibName (List exprs) =
    LibName
        <$> mapM
            ( \case
                Id i -> Right i
                -- TODO: Only allow <uinteger 10> in library name
                Number n -> Right $ T.pack (show n :: String)
                e -> makeErr e "expected identifier or uinteger"
            )
            exprs
mkLibName e = makeErr e "expected non-empty list"

------------------------------------------------------------------------

-- | Export specification with internal name and external name.
-- For exports which are not renamed both are the same.
data ExportSpec = Export {internal :: T.Text, external :: T.Text}
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
mkExport (List ((Id "export") : items)) =
    foldr
        ( \x acc -> case exportDecl x of
            Right e -> fmap (e :) acc
            Left e -> Left e
        )
        (Right [])
        items
mkExport e = makeErr e "expected export list"

-- Find an export expression within a library declaration.
-- May return an empty list if no export declaration was found.
findExport :: [Sexp] -> Either SyntaxError [ExportSpec]
findExport (e@(List ((Id "export") : _)) : _) = mkExport e
findExport (_ : exprs) = findExport exprs
findExport [] = Right []
