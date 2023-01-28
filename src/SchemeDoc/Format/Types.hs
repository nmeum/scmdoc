module SchemeDoc.Format.Types
    (Declaration(..), Formatable(..), Formatter)
where

import Text.Blaze.Html
import qualified Data.Text as T

import SchemeDoc.Types

-- Type class to convert a given type to a formatted S-expression.
class Formatable a where
    fmt :: a -> T.Text -> Declaration

-- A declaration in the Scheme source code (e.g. a library declaration).
data Declaration = Declaration { declId   :: T.Text
                               , declDesc :: T.Text
                               , declFmt  :: Html }

-- Type to convert an S-expression into a unique id and a formatter.
type Formatter = Sexp -> T.Text -> Maybe Declaration
