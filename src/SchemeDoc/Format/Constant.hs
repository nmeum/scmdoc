module SchemeDoc.Format.Constant where

import SchemeDoc.Types
import SchemeDoc.Format.Types
import SchemeDoc.Format.Util

import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H

data Constant = Constant { consName  :: String
                         , consValue :: Sexp }
    deriving (Show)

instance Formatable Constant where
    fmt (Constant n _) = Format n
        (\comment -> do
                        component "constant " n
                        H.p $ toHtml comment
                        htmlSexp (Id n))

-- Parses a Scheme definition.
--
--  <definition> → (define <identifier> <expression>)
--
mkConstant :: Sexp -> Maybe Constant
mkConstant (List ((Id "define"):(Id name):expr:[])) =
    Just $ Constant name expr
mkConstant _ = Nothing
