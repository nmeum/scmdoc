{-# LANGUAGE OverloadedStrings #-}
module SchemeDoc.Format.Types
    (Declaration(..), Formatable(..), Formatter,
     Section(..), sectionComment, defaultSection, sectionFmt,
     Component(..), compAnchor, compLink)
where

import SchemeDoc.Types
import SchemeDoc.Util (ltrim)
import Data.Char (isSpace)
import qualified Data.Text as T

import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- Type class to convert a given type to a formatted S-expression.
class Formatable a where
    fmt :: a -> T.Text -> Declaration

-- A declaration in the Scheme source code (e.g. a library declaration).
data Declaration = Declaration { declId   :: T.Text
                               , declDesc :: T.Text
                               , declFmt  :: Html }

-- Type to convert an S-expression into a unique id and a formatter.
type Formatter = Sexp -> T.Text -> Maybe Declaration

------------------------------------------------------------------------

-- Section represents a section comment in the source.
-- Each section comment has a title and a description.
data Section = Section T.Text T.Text

-- Character used to identify section comments.
sectionChar :: Char
sectionChar = '|'

-- If the given Text constitutes a section comment regarding
-- the section title. Othwerwise, return Nothing.
sectionComment :: T.Text -> Maybe T.Text
sectionComment t = if T.length t >= 1 && T.head t == sectionChar
                       then Just (ltrim $ T.tail t)
                       else Nothing

-- Default section, used if the input file doesn't contain
-- a single section comment of its own.
defaultSection :: Section
defaultSection = Section "Declarations" ""

-- Format a section comment as HTML.
sectionFmt :: Section -> Html
sectionFmt s@(Section n desc) = do
    -- TODO: Add <section> tags for each H2
    H.h2 ! A.id (textValue $ compAnchor (S s))
         $ (toHtml n)
    H.p  (toHtml desc)

------------------------------------------------------------------------

-- Comment in the documented source code.
-- This is either a declaration or a section comment.
data Component = D Declaration | S Section

-- Return unique anchor for a compoment.
compAnchor :: Component -> T.Text
compAnchor (D c) = declId c -- TODO: Ensure that this is aligned with format function
compAnchor (S (Section n _)) = "section-" `T.append` toAnchor n
  where
    toAnchor :: T.Text -> T.Text
    toAnchor = T.map (\c -> if isSpace c then '-' else c) . T.toLower

-- Generate an anchor tag which links to a given compoment.
compLink :: Component -> Html
compLink c = do
    H.a ! A.href (textValue $ T.cons '#' (compAnchor c))
        $ (toHtml $ compName c)
  where
    compName :: Component -> T.Text
    compName (D c') = declId c'
    compName (S (Section n _)) = n
