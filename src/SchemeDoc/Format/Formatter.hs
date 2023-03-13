{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module provides functions for applying a 'Formatter' to
-- create 'Component's for given S-expressions. Furthermore, an
-- HTML document, for recognized 'Component's, can be generated
-- using this module.
module SchemeDoc.Format.Formatter (findComponents, format, defFormatter)
where

import Control.Applicative
import Control.Monad

import qualified SchemeDoc.Format.Library as L
import SchemeDoc.Format.Procedure
import SchemeDoc.Format.Syntax
import SchemeDoc.Format.Types
import SchemeDoc.Format.Variable
import SchemeDoc.Types

import qualified Data.Text as T
import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H

-- ToC represents a table of contents with a depth of one.
data ToC = Heading Section | Items [Declaration]

tableOfContents' :: [Component] -> [ToC] -> [ToC]
tableOfContents' ((D pc) : xs) ((Items lst) : toc) = tableOfContents' xs (toc ++ [Items $ lst ++ [pc]])
tableOfContents' ((D pc) : xs) toc = tableOfContents' xs (toc ++ [Items [pc]])
tableOfContents' ((S sec) : xs) toc = tableOfContents' xs (toc ++ [Heading sec])
tableOfContents' [] toc = toc

tableOfContents :: [Component] -> Html
tableOfContents comps = H.ul $ do
    forM_
        (tableOfContents' comps [])
        ( \case
            Heading s -> H.li (compLink $ S s)
            Items lst -> H.ul (formatItems lst)
        )
  where
    formatItems :: [Declaration] -> Html
    formatItems decls = forM_ decls (H.li . compLink . D)

------------------------------------------------------------------------

-- | The default 'Formatter', can be extented via the 'Maybe' applicative.
defFormatter :: Sexp -> T.Text -> Maybe Declaration
defFormatter sexp desc =
    flip fmt desc <$> mkVariable sexp
        <|> flip fmt desc <$> mkProcedure sexp
        <|> flip fmt desc <$> mkSyntax sexp

-- | Find all 'Component's recognized by the given 'Formatter'.
-- Non-recognized S-expressions are also returned.
findComponents :: Formatter -> [Documented] -> ([Component], [Sexp])
findComponents formatFn = foldl foldFunc ([], [])
  where
    foldFunc (acc, unFmt) (secRaw, com@(DocComment desc)) =
        case sectionComment secRaw of
            Just sec -> let s = Section sec desc in (acc ++ [S s], unFmt)
            Nothing -> (acc, unFmt ++ [com])
    foldFunc (acc, unFmt) (desc, expr) =
        case formatFn expr desc of
            Just c -> (acc ++ [D c], unFmt)
            Nothing -> (acc, unFmt ++ [expr])

-- | Format all 'Component's which are exported by the given 'L.Library'.
format :: L.Library -> [Component] -> Html
format lib comps = do
    H.h2 "Index"
    H.details $ do
        H.summary "Table of contents"
        tableOfContents finalComps
    forM_
        finalComps
        ( \case
            D c -> declFmt c (L.externalId lib $ declId c)
            S s -> sectionFmt s
        )
  where
    -- Exclude any non-exported program components.
    exportedComps =
        filter
            ( \case
                D c -> L.exports lib $ declId c
                S _ -> True
            )
            comps

    -- Extra components to prepend to the component list.
    -- Neccessary to ensure that HTML heading structure is always valid.
    extraComps =
        if any (\case D _ -> False; S _ -> True) exportedComps
            then []
            else [S defaultSection]

    -- Prepend extraComps to the exported components.
    finalComps = extraComps ++ exportedComps
