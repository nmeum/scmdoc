module SchemeDoc.Format.Util where

import SchemeDoc.Types
import Text.Blaze.Html

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- Fromat an S-expression in an HTML code block.
htmlSexp :: Sexp -> Html
htmlSexp = H.pre . H.code . toHtml . show

component :: String -> String -> Html
component prefix name = do
    H.h3 $ do
        toHtml $ prefix ++ " "
        H.a ! A.name (stringValue name) ! A.href (stringValue $ "#" ++ name) $ toHtml name
