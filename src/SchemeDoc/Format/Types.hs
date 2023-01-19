module SchemeDoc.Format.Types
    (FormatF, Format(..), Formatable(..), Formatter)
where

import Text.Blaze.Html
import qualified Data.Text as T

import SchemeDoc.Types

-- A function for formatting an S-expressions.
-- Receives the documentation comment for the expression as an input.
type FormatF = (T.Text -> Html)

-- A Scheme expression prepared for formatting.
data Format = Format { fmtId   :: T.Text
                     , fmtFunc :: FormatF }

-- Type class to convert a given type to a formatted S-expression.
class Formatable a where
    fmt :: a -> Format

-- Type to convert an S-expression into a unique id and a formatter.
type Formatter = Sexp -> Maybe Format
