{-# LANGUAGE OverloadedStrings #-}
module SchemeDoc.Format.Util where

import CMark
import SchemeDoc.Types
import Text.Blaze.Html
import qualified Data.Text as T

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- Fromat an S-expression in an HTML code block.
htmlSexp :: Sexp -> Html
htmlSexp = H.pre . H.code . toHtml . show

component :: T.Text -> T.Text -> Html
component prefix name = do
    H.h3 $ do
        toHtml $ (toMarkup $ T.append prefix " ")
        H.a ! A.id (textValue name)
            ! A.href (textValue (T.cons '#' name))
            $ toHtml name

-- Convert from Markdown to Html.
fromMkd :: T.Text -> Html
fromMkd s = preEscapedToHtml $ commonmarkToHtml [] s
