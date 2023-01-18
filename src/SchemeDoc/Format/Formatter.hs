{-# LANGUAGE LambdaCase #-}
module SchemeDoc.Format.Formatter
where

import Prelude hiding (map)
import Data.Char (isSpace)
import Data.Text hiding (filter, foldl, foldr, length, head)
import Control.Applicative

import SchemeDoc.Types
import SchemeDoc.Format.Types
import SchemeDoc.Format.Procedure
import SchemeDoc.Format.Constant
import SchemeDoc.Format.Library

import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- Program component (i.e. S-expression) suitable for formatting.
-- For example, procedure definitions, library declarations, et cetera.
data ProgComp = ProgComp { compId     :: Text
                         , compParent :: Section
                         , compDesc   :: Text
                         , compFunc   :: FormatF }

-- Section represents a section comment in the source.
-- Each section comment has a title and a description.
data Section = Section Text Text

-- Comment in the documented source code.
-- This is either a program component (i.e. an S-expression) or a section comment.
data Component = P ProgComp | S Section

-- Return unique anchor for a compoment.
compAnchor :: Component -> Text
compAnchor (P c) = compId c -- TODO: Ensure that this is aligned with format function
compAnchor (S (Section n _)) = (pack "section-") `append` toAnchor n
  where
    toAnchor :: Text -> Text
    toAnchor = map (\c -> if isSpace c then '-' else c) . toLower

-- Generate an anchor tag which links to a given compoment.
compLink :: Component -> Html
compLink c = do
    H.a ! A.href (textValue $ append (pack "#") (compAnchor c))
        $ (toHtml $ compName c)
  where
    compName :: Component -> Text
    compName (P c') = compId c'
    compName (S (Section n _)) = n

-- Format a program component as HTML.
compFormat :: ProgComp -> Html
compFormat c = (compFunc c) $ compDesc c

-- Format a section comment as HTML.
sectionFormat :: Section -> Html
sectionFormat s@(Section n desc) = do
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
formatProgComps = foldl (\acc pc -> acc >> (H.li $ compLink (P pc)))
                        (toHtml "")

tableOfContents :: [Component] -> Html
tableOfContents comps = H.ul $ do
    foldl (\acc t -> case t of
        Heading s   -> acc >> H.li (compLink $ S s)
        Items   lst -> acc >> H.ul (formatProgComps lst))
          (toHtml "") toc
  where
    toc = tableOfContents' comps []

------------------------------------------------------------------------

-- The default Formatter, can be extented via the Maybe applicative.
defFormatter :: Sexp -> Maybe Format
defFormatter sexp = fmt <$> mkConstant sexp
                <|> fmt <$> mkProcedure sexp

mkProgComp :: Formatter -> Section -> Text -> Sexp -> Maybe ProgComp
mkProgComp f parent desc expr = f expr >>=
    (\(Format i fn) -> Just $ ProgComp i parent desc fn)

findComponents :: Section -> Formatter -> [Documented] -> ([Component], [Sexp])
findComponents root formatFn docs =
    let (a, _, c) = foldl format' ([], root, []) docs in (a, c)
  where
    format' (acc, _, unFmt) (sec, (DocComment desc)) =
        -- TODO: Check if sec has the format "| text", e.g. ";;>| My Section"
        let s = Section sec desc in (acc ++ [S s], s, unFmt)
    format' (acc, p, unFmt) (desc, expr) =
        case mkProgComp formatFn p desc expr of
            Just c   -> (acc ++ [P c], p, unFmt)
            Nothing  -> (acc, p, unFmt ++ [expr])

-- Format all documented S-expressions which are exported
-- by the given Scheme library using the given Formatter.
--
-- Returns pair of HTML for succesfully formatted S-expressions
-- and list of S-expressions which are documented but for which
-- no formatter was found.
format :: Library -> Formatter -> [Documented] -> (Html, [Sexp])
format lib fmtF docs = ( do
                            H.h2 (toHtml "Index")
                            H.details $ do
                                H.summary (toHtml "Table of contents")
                                tableOfContents exportedComps
                            foldl (\acc comp -> case comp of
                                      P c -> acc >> compFormat c
                                      S s -> acc >> sectionFormat s
                                  ) (toHtml "") exportedComps
                       , unFmt )
  where
    (comps, unFmt) = findComponents (Section (pack "") (pack "")) fmtF docs
    exportedComps  = filter (\case
                                P c -> libExports lib $ compId c
                                S _ -> True) comps
