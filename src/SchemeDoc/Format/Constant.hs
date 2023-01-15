{-# LANGUAGE OverloadedStrings #-}
module SchemeDoc.Format.Constant where

import Data.Text

import SchemeDoc.Types
import SchemeDoc.Format.Types
import SchemeDoc.Format.Util

data Constant = Constant { consName  :: Text
                         , consValue :: Sexp }
    deriving (Show)

instance Formatable Constant where
    fmt (Constant n _) = Format n
        (\comment -> do
                        component "constant " n
                        fromMkd comment
                        htmlSexp (Id n))

-- Parses a Scheme definition.
--
--  <definition> → (define <identifier> <expression>)
--
mkConstant :: Sexp -> Maybe Constant
mkConstant (List ((Id "define"):(Id name):expr:[])) =
    Just $ Constant name expr
mkConstant _ = Nothing
