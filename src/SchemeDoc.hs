{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module SchemeDoc
    (DocLib, findDocLibs, docDecls, docFmt, mkDoc, findUndocumented)
where

import SchemeDoc.Types
import SchemeDoc.Error
import SchemeDoc.Format.Types
import SchemeDoc.Format.Library
import SchemeDoc.Format.Formatter

import Text.Blaze.Html
import Text.Blaze.Html.Renderer.String
import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- A documented Scheme library.
type DocLib = (T.Text, Library)

-- Find all documented Scheme library declarations in a Scheme source.
findDocLibs :: [Sexp] -> Either SyntaxError [DocLib]
findDocLibs exprs = foldr fn (Right []) (findDocumented exprs)
    where
        fn (s, e@(List ((Id "define-library"):_))) acc =
            case mkLibrary e of
                Right lib -> fmap ((:) (s, lib)) acc
                Left err  -> Left err
        fn _ acc = acc

-- Find all documented declarations of a library.
-- Performs file system accesses to expand includes.
docDecls :: DocLib -> IO ([Component], [Sexp])
docDecls (_, lib) = do
    sexprs <- libExpand lib
    pure $ findComponents defFormatter (findDocumented sexprs)

-- Expand a documented library wrt. its declarations.
-- Returns resulting HTML and list of S-expressions
-- for which no formatter was found.
docFmt :: DocLib -> [Component] -> Html
docFmt (libDesc, lib) comps =
    let html = format lib comps in
        (declFmt $ fmt lib libDesc) >> html

-- Create an HTML document with the given title, stylesheet, and body.
mkDoc :: String -> String -> Html -> String
mkDoc title css hbody = renderHtml $ H.docTypeHtml $ do
    H.head $ do
        H.meta ! A.charset "UTF-8"
        H.meta ! A.name "viewport"
               ! A.content "width=device-width, initial-scale=1"

        H.link ! A.rel "stylesheet"
               ! A.href (stringValue css)
        H.title $ toHtml title
    H.body hbody

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
        toPairLst :: [Sexp] -> [(Documented)]
        toPairLst [] = []
        toPairLst ((DocComment s):expr:xs) = (s, expr) : toPairLst xs
        toPairLst _ = error "unreachable"

-- Find all identifiers which are exported but not documented.
findUndocumented :: Library -> [Component] -> [T.Text]
findUndocumented lib comps = filter (\i -> not $ member i comps)
                                $ map internal (libExport lib)
  where
    member :: T.Text -> [Component] -> Bool
    member ident = any (\case
            D Declaration{declId=i} -> i == ident
            S _ -> False)
