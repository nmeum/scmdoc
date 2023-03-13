{-# LANGUAGE OverloadedStrings #-}

-- | This module implmeents a 'Formatter' for Scheme syntax definitions.
module SchemeDoc.Format.Syntax (Syntax, mkSyntax) where

import qualified Data.Text as T
import SchemeDoc.Format.Types
import SchemeDoc.Format.Util
import SchemeDoc.Types

-- | A R7RS syntax definition.
newtype Syntax = Syntax
    { name :: T.Text
    -- ^ Name of the syntax definition
    }
    deriving (Eq, Show)

instance Formatable Syntax where
    -- For syntax definitions, usage instruction should be
    -- provided in the accompanying documentation comment.
    fmt (Syntax keyword) desc =
        mkDeclaration keyword desc $ \n ->
            do
                component "syntax " n
                fromMkd desc

-- | Parse a Scheme syntax definition
--
-- > <syntax definition> →
-- >   (define-syntax <keyword> <transformer spec>)
mkSyntax :: Sexp -> Maybe Syntax
mkSyntax (List (Id "define-syntax" : Id keyword : _)) =
    Just $ Syntax keyword
mkSyntax _ = Nothing
