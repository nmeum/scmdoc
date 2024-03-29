{-# LANGUAGE OverloadedStrings #-}

-- | This module implements a 'Formatter' for Scheme variable definition.
--
-- For example:
--
-- > (define x 42)
module SchemeDoc.Format.Variable (Variable (..), mkVariable) where

import qualified Data.Text as T
import SchemeDoc.Format.Types
import SchemeDoc.Format.Util
import SchemeDoc.Types

-- | A R7RS Scheme variable definition.
data Variable = Variable
    { name :: T.Text
    -- ^ Identifier, i.e. variable name.
    , value :: Sexp
    -- ^ Value assigned to the variable name.
    }
    deriving (Eq, Show)

instance Formatable Variable where
    fmt (Variable internalId _) desc =
        mkDeclaration internalId desc $ \n ->
            do
                component "constant " n
                fromMkd desc
                htmlSexp (Id n)

-- | Parses a Scheme definition.
--
-- > <definition> → (define <identifier> <expression>)
mkVariable :: Sexp -> Maybe Variable
mkVariable (List [Id "define", Id n, expr]) =
    Just $ Variable n expr
mkVariable _ = Nothing
