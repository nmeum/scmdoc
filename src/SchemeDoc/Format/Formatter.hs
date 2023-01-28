{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module SchemeDoc.Format.Formatter
    (findComponents, format, defFormatter, Component(..), Section(..), ProgComp(..))
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

-- Program component (i.e. S-expression) suitable for formatting.
-- For example, procedure definitions, library declarations, et cetera.
data ProgComp = ProgComp { compId     :: T.Text
                         , compParent :: Section
                         , compDesc   :: T.Text
                         , compFunc   :: FormatF }

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

-- Comment in the documented source code.
-- This is either a program component (i.e. an S-expression) or a section comment.
data Component = P ProgComp | S Section

-- Return unique anchor for a compoment.
compAnchor :: Component -> T.Text
compAnchor (P c) = compId c -- TODO: Ensure that this is aligned with format function
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
    compName (P c') = compId c'
    compName (S (Section n _)) = n

-- Format a program component as HTML.
compFormat :: ProgComp -> Html
compFormat c = (compFunc c) $ compDesc c

-- Format a section comment as HTML.
sectionFormat :: Section -> Html
sectionFormat s@(Section n desc) = do
    -- TODO: Add <section> tags for each H2
    H.h2 ! A.id (textValue $ compAnchor (S s))
         $ (toHtml n)
    H.p  (toHtml desc)

------------------------------------------------------------------------

-- ToC represents a table of contents with a depth of one.
data ToC = Heading Section | Items [ProgComp]

tableOfContents' :: [Component] -> [ToC] -> [ToC]
tableOfContents' ((P pc):xs) ((Items lst):toc) = tableOfContents' xs (toc ++ [(Items $ lst ++ [pc])])
tableOfContents' ((P pc):xs) toc               = tableOfContents' xs (toc ++ [(Items [pc])])
tableOfContents' ((S sec):xs) toc              = tableOfContents' xs (toc ++ [(Heading sec)])
tableOfContents' [] toc                        = toc

formatProgComps :: [ProgComp] -> Html
formatProgComps comps = forM_ comps (\pc -> H.li $ compLink (P pc))

tableOfContents :: [Component] -> Html
tableOfContents comps = H.ul $ do
    forM_ toc (\t -> case t of
        Heading s   -> H.li (compLink $ S s)
        Items   lst -> H.ul (formatProgComps lst))
  where
    toc = tableOfContents' comps []

------------------------------------------------------------------------

-- The default Formatter, can be extented via the Maybe applicative.
defFormatter :: Sexp -> Maybe Format
defFormatter sexp = fmt <$> mkConstant sexp
                <|> fmt <$> mkProcedure sexp

mkProgComp :: Formatter -> Section -> T.Text -> Sexp -> Maybe ProgComp
mkProgComp f parent desc expr = f expr >>=
    (\(Format i fn) -> Just $ ProgComp i parent desc fn)

findComponents' :: Section -> Formatter -> [Documented] -> ([Component], [Sexp])
findComponents' root formatFn docs =
    let (a, _, c) = foldl format' ([], root, []) docs in (a, c)
  where
    format' (acc, p, unFmt) (secRaw, com@(DocComment desc)) =
        case sectionComment secRaw of
            Just sec -> let s = Section sec desc in (acc ++ [S s], s, unFmt)
            Nothing  -> (acc, p, unFmt ++ [com])
    format' (acc, p, unFmt) (desc, expr) =
        case mkProgComp formatFn p desc expr of
            Just c   -> (acc ++ [P c], p, unFmt)
            Nothing  -> (acc, p, unFmt ++ [expr])

-- Find all components recognized by the given formatter.
-- Non-recognized S-expressions are also returned.
findComponents :: Formatter -> [Documented] -> ([Component], [Sexp])
findComponents = findComponents' defaultSection

-- Format all components which are exported by the given library.
format :: Library -> [Component] -> Html
format lib comps = do
    H.h2 "Index"
    H.details $ do
        H.summary "Table of contents"
        tableOfContents finalComps
    forM_ finalComps (\case
              P c -> compFormat c
              S s -> sectionFormat s)
  where
    -- Exclude any non-exported program components.
    exportedComps = filter (\case
                                P c -> libExports lib $ compId c
                                S _ -> True) comps

    -- Extra components to prepend to the component list.
    -- Neccessary to ensure that HTML heading structure is always valid.
    extraComps = if any (\case { P _ -> False ;  S _ -> True }) exportedComps
                        then []
                        else [S defaultSection]

    -- Prepend extraComps to the exported components.
    finalComps = extraComps ++ exportedComps
