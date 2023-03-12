{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This is the high-level interface for rendering Scheme source
-- documentation to a suitable output format. Currently, only HTML
-- is supported as an output format.
--
-- The API, provided by this module, is structured around R7RS
-- Scheme 'L.Library' declarations and generates documentation
-- for all exported identifiers of a Scheme library.
module SchemeDoc (DocLib, findDocLibs, docDecls, docFmt, mkDoc, findUndocumented)
where

import SchemeDoc.Error
import SchemeDoc.Format.Formatter
import qualified SchemeDoc.Format.Library as L
import SchemeDoc.Format.Types
import SchemeDoc.Types

import qualified Data.Text as T
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.String
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- | A documented Scheme library, i.e. a Scheme 'L.Library' declaration
-- which is proceded by a 'DocComment'.
type DocLib = (T.Text, L.Library)

-- | Find all documented Scheme 'L.Library' declarations in a Scheme source.
findDocLibs :: [Sexp] -> Either SyntaxError [DocLib]
findDocLibs exprs = foldr fn (Right []) (findDocumented exprs)
  where
    fn (s, e@(List ((Id "define-library") : _))) acc =
        case L.mkLibrary e of
            Right lib -> fmap ((s, lib) :) acc
            Left err -> Left err
    fn _ acc = acc

-- | Find all documented 'Component's of a Scheme 'L.Library'. Performs
-- file system access to expand includes and may return an 'ErrParser'
-- exception.
--
-- Returns the recognized Components and a list of S-expressions which
-- were preceded by a 'DocComment' but for which no suitable 'Formatter'
-- was found.
docDecls :: DocLib -> IO ([Component], [Sexp])
docDecls (_, lib) = do
    sexprs <- L.expand lib
    pure $ findComponents defFormatter (findDocumented sexprs)

-- | Format a documented 'L.Library', with regards to its 'Component's
-- (obtained via 'docDecls') as an 'Html' document.
docFmt :: DocLib -> [Component] -> Html
docFmt (libDesc, lib) comps =
    let html = format lib comps
     in declFmt (fmt lib libDesc) Nothing >> html

-- | Render an 'Html' document with the given title, stylesheet, and body.
mkDoc :: String -> String -> Html -> String
mkDoc title css hbody = renderHtml $ H.docTypeHtml $ do
    H.head $ do
        H.meta ! A.charset "UTF-8"
        H.meta
            ! A.name "viewport"
            ! A.content "width=device-width, initial-scale=1"

        H.link
            ! A.rel "stylesheet"
            ! A.href (stringValue css)
        H.title $ toHtml title
    H.body hbody

-- | The name of all internal identifiers which are exported by the
-- 'L.Library' but not documented, i.e. not preceded by a 'DocComment'.
findUndocumented :: L.Library -> [Component] -> [T.Text]
findUndocumented lib comps =
    filter (\i -> not $ member i comps) $
        map L.internal (L.exported lib)
  where
    member :: T.Text -> [Component] -> Bool
    member ident =
        any
            ( \case
                D d -> declId d == ident
                S _ -> False
            )

------------------------------------------------------------------------

-- Filter all non-documented S-expressions.
filterDocs :: [Sexp] -> [Sexp]
filterDocs = snd . walk filterDocs' (False, [])
  where
    filterDocs' :: (Bool, [Sexp]) -> Sexp -> (Bool, [Sexp])
    filterDocs' (False, acc) c@(DocComment _) = (True, acc ++ [c])
    filterDocs' (True, acc) expr = (False, acc ++ [expr])
    filterDocs' b _ = b

-- Find all S-expressions which are preceded by a documentation comment.
findDocumented :: [Sexp] -> [Documented]
findDocumented = toPairLst . filterDocs
  where
    toPairLst :: [Sexp] -> [Documented]
    toPairLst [] = []
    toPairLst ((DocComment s) : expr : xs) = (s, expr) : toPairLst xs
    toPairLst _ = error "unreachable"
