{-# LANGUAGE OverloadedStrings #-}

-- | This module implements a 'Formatter' for Scheme variable definition.
--
-- For example:
--
-- > (define x 42)
--
module SchemeDoc.Format.Variable where

import SchemeDoc.Types
import SchemeDoc.Format.Types
import SchemeDoc.Format.Util
import qualified Data.Text as T

-- | A R7RS Scheme variable definition.
data Variable = Variable { varName  :: T.Text -- ^ Identifier, i.e. variable name.
                         , varValue :: Sexp   -- ^ Value assigned to the variable name.
                         }
    deriving (Eq, Show)

instance Formatable Variable where
    fmt (Variable n _) desc =
        Declaration n desc $ do
                        component "constant " n
                        fromMkd desc
                        htmlSexp (Id n)

-- | Parses a Scheme definition.
--
-- > <definition> → (define <identifier> <expression>)
--
mkVariable :: Sexp -> Maybe Variable
mkVariable (List ((Id "define"):(Id name):expr:[])) =
    Just $ Variable name expr
mkVariable _ = Nothing
