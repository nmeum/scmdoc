{-# LANGUAGE OverloadedStrings #-}

-- | Utility functions for creating custom 'SchemeDoc.Format.Types.Formatter's.
module SchemeDoc.Format.Util where

import CMarkGFM
import qualified Data.Text as T
import SchemeDoc.Types
import Text.Blaze.Html

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- | Format an S-expression in an 'Html' code block.
htmlSexp :: Sexp -> Html
htmlSexp = H.pre . H.code . toHtml . show

-- | Format a component with the given type prefix and the given name.
component :: T.Text -> T.Text -> Html
component prefix name = do
    H.h3 $ do
        toHtml $ (toMarkup $ T.append prefix " ")
        H.a
            ! A.id (textValue name)
            ! A.href (textValue (T.cons '#' name))
            $ toHtml name

-- | Convert from Markdown to Html.
fromMkd :: T.Text -> Html
fromMkd s = preEscapedToHtml $ commonmarkToHtml [] [] s
