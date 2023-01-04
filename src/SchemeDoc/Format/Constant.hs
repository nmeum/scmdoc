module SchemeDoc.Format.Constant where

import SchemeDoc
import SchemeDoc.Format.Types

data Constant = Constant { consName  :: String
                         , consValue :: Sexp }

instance Formatable Constant where
    fmt (Constant n _) = Formatted "constant" n $ Id n

mkConstant :: Sexp -> Maybe Constant
mkConstant _ = Nothing
