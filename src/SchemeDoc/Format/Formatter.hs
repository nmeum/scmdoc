{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module SchemeDoc.Format.Formatter
    (findComponents, format, defFormatter, Component(..), Section(..))
where

import Data.Char (isSpace)
import Control.Applicative
import Control.Monad

import SchemeDoc.Types
import SchemeDoc.Util (ltrim)
import SchemeDoc.Format.Types
import SchemeDoc.Format.Procedure
import SchemeDoc.Format.Constant
import SchemeDoc.Format.Library

import Text.Blaze.Html
import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- Section represents a section comment in the source.
-- Each section comment has a title and a description.
data Section = Section T.Text T.Text

-- Default section, used if the input file doesn't contain
-- a single section comment of its own.
defaultSection :: Section
defaultSection = Section "Declarations" ""

-- Character used to identify section comments.
sectionChar :: Char
sectionChar = '|'

-- If the given Text constitutes a section comment regarding
-- the section title. Othwerwise, return Nothing.
sectionComment :: T.Text -> Maybe T.Text
sectionComment t = if T.length t >= 1 && T.head t == sectionChar
                       then Just (ltrim $ T.tail t)
                       else Nothing

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

------------------------------------------------------------------------

-- ToC represents a table of contents with a depth of one.
data ToC = Heading Section | Items [Declaration]

tableOfContents' :: [Component] -> [ToC] -> [ToC]
tableOfContents' ((D pc):xs) ((Items lst):toc) = tableOfContents' xs (toc ++ [(Items $ lst ++ [pc])])
tableOfContents' ((D pc):xs) toc               = tableOfContents' xs (toc ++ [(Items [pc])])
tableOfContents' ((S sec):xs) toc              = tableOfContents' xs (toc ++ [(Heading sec)])
tableOfContents' [] toc                        = toc

tableOfContents :: [Component] -> Html
tableOfContents comps = H.ul $ do
    forM_ (tableOfContents' comps []) (\case
        Heading s   -> H.li (compLink $ S s)
        Items   lst -> H.ul (formatItems lst))
  where
    formatItems :: [Declaration] -> Html
    formatItems decls = forM_ decls (\pc -> H.li $ compLink (D pc))

------------------------------------------------------------------------

-- The default Formatter, can be extented via the Maybe applicative.
defFormatter :: Sexp -> T.Text -> Maybe Declaration
defFormatter sexp desc = ((flip fmt) desc) <$> mkConstant sexp
                     <|> ((flip fmt) desc) <$> mkProcedure sexp

-- Find all components recognized by the given formatter.
-- Non-recognized S-expressions are also returned.
findComponents :: Formatter -> [Documented] -> ([Component], [Sexp])
findComponents formatFn docs = foldl foldFunc ([], []) docs
  where
    foldFunc (acc, unFmt) (secRaw, com@(DocComment desc)) =
        case sectionComment secRaw of
            Just sec -> let s = Section sec desc in (acc ++ [S s], unFmt)
            Nothing  -> (acc, unFmt ++ [com])
    foldFunc (acc, unFmt) (desc, expr) =
        case formatFn expr desc of
            Just c   -> (acc ++ [D c], unFmt)
            Nothing  -> (acc, unFmt ++ [expr])

-- Format all components which are exported by the given library.
format :: Library -> [Component] -> Html
format lib comps = do
    H.h2 "Index"
    H.details $ do
        H.summary "Table of contents"
        tableOfContents finalComps
    forM_ finalComps (\case
              D c -> declFmt c
              S s -> sectionFmt s)
  where
    -- Exclude any non-exported program components.
    exportedComps = filter (\case
                                D c -> libExports lib $ declId c
                                S _ -> True) comps

    -- Extra components to prepend to the component list.
    -- Neccessary to ensure that HTML heading structure is always valid.
    extraComps = if any (\case { D _ -> False ;  S _ -> True }) exportedComps
                        then []
                        else [S defaultSection]

    -- Prepend extraComps to the exported components.
    finalComps = extraComps ++ exportedComps
