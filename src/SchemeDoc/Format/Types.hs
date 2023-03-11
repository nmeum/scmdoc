{-# LANGUAGE OverloadedStrings #-}

-- | This module provides data types for building 'Formatter's.
module SchemeDoc.Format.Types (
    Declaration (..),
    Formatable (..),
    Formatter,
    Section (..),
    sectionComment,
    defaultSection,
    sectionFmt,
    Component (..),
    compAnchor,
    compLink,
)
where

import Control.Monad (unless)
import Data.Char (isSpace)
import qualified Data.Text as T
import SchemeDoc.Types

import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- | Type class to convert a given type to a formatted S-expression.
class Formatable a where
    -- Create a 'Declaration' using the given documentation.
    fmt :: a -> T.Text -> Declaration

-- | A high-level declaration in the Scheme source code, e.g. a
-- 'SchemeDoc.Format.Library' declaration.
data Declaration = Declaration
    { declId :: T.Text
    -- ^ Internal identifier of the declaration.
    , declDesc :: T.Text
    -- ^ Documentation for the declaration, i.e. the 'DocComment' preceding it.
    , declFmt :: Html
    -- ^ HTML documenation for the declaration.
    }

-- | Function for converting an S-expression into a 'Declaration'
-- based on the given documentation for the S-expression.
type Formatter = Sexp -> T.Text -> Maybe Declaration

------------------------------------------------------------------------

-- | Section represents a section comment in the source.
data Section
    = Section
        T.Text
        -- ^ Section title.
        T.Text
        -- ^ Sectiond description.

-- Character used to identify section comments.
sectionChar :: Char
sectionChar = '|'

-- Remove all leading ASCII space characters.
ltrim :: T.Text -> T.Text
ltrim = T.dropWhile isSpace

-- Remove all trailing ASCII space characters.
rtrim :: T.Text -> T.Text
rtrim = T.dropWhileEnd isSpace

-- Remove all trailing and leading ASCII space characters.
trim :: T.Text -> T.Text
trim = rtrim . ltrim

-- | If the given comment text constitutes a 'Section' title
-- return the title, otherwise return 'Nothing'.
sectionComment :: T.Text -> Maybe T.Text
sectionComment t =
    if T.length t >= 1 && T.head t == sectionChar
        then Just (ltrim $ T.tail t)
        else Nothing

-- | Default section, used if the input file doesn't contain a single
-- section comment of its own.
defaultSection :: Section
defaultSection = Section "Declarations" ""

-- | Format a section comment as 'Html'.
sectionFmt :: Section -> Html
sectionFmt s@(Section n desc) = do
    -- TODO: Add <section> tags for each H2
    H.h2 ! A.id (textValue $ compAnchor (S s)) $
        (toHtml n)
    unless (T.null desc) $ do
        H.p $ toHtml desc

------------------------------------------------------------------------

-- | Component in the documented source code.
-- This is either a 'Declaration' or a 'Section' comment.
data Component = D Declaration | S Section

-- | Unique anchor for a compoment.
compAnchor :: Component -> T.Text
compAnchor (D c) = declId c -- TODO: Ensure that this is aligned with format function
compAnchor (S (Section n _)) = "section-" `T.append` toAnchor n
  where
    toAnchor :: T.Text -> T.Text
    toAnchor = T.map (\c -> if isSpace c then '-' else c) . T.toLower . trim

-- | Generate an anchor tag which links to a given 'Component'.
compLink :: Component -> Html
compLink c = do
    H.a ! A.href (textValue $ T.cons '#' (compAnchor c)) $
        (toHtml $ compName c)
  where
    compName :: Component -> T.Text
    compName (D c') = declId c'
    compName (S (Section n _)) = n
