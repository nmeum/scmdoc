module SchemeDoc.Format.Types
    (Formatted(..), Formatable(..), Formatter)
where

import SchemeDoc

-- A formatted S-expressions.
data Formatted = Formatted { objDesc :: String -- e.g. procedure
                           , objName :: String -- e.g. my-func
                           , objExpr :: Sexp }

-- Type class to convert a given type to a formatted S-expression.
class Formatable a where
    fmt :: a -> Formatted

-- The default formatter for S-expression this is only intended to
-- be used as a catch-all fallback formatter.
instance Formatable Sexp where
    fmt expr = Formatted "expression" "expr" expr

-- Type to convert an S-expression into a Formatted value.
type Formatter = Sexp -> Maybe Formatted
