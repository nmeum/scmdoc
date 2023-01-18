{-# LANGUAGE OverloadedStrings #-}
module SchemeDoc.Format.Util where

import Data.Text
import SchemeDoc.Types
import Text.Blaze.Html
import CMark

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- Fromat an S-expression in an HTML code block.
htmlSexp :: Sexp -> Html
htmlSexp = H.pre . H.code . toHtml . show

component :: Text -> Text -> Html
component prefix name = do
    H.h3 $ do
        toHtml $ (toMarkup $ append prefix " ")
        H.a ! A.id (textValue name)
            ! A.href (textValue (cons '#' name))
            $ toHtml name

-- Convert from Markdown to Html.
fromMkd :: Text -> Html
fromMkd s = preEscapedToHtml $ commonmarkToHtml [] s
