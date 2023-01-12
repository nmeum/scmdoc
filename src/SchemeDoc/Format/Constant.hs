module SchemeDoc.Format.Constant where

import SchemeDoc.Types
import SchemeDoc.Output
import SchemeDoc.Format.Types

data Constant = Constant { consName  :: String
                         , consValue :: Sexp }
    deriving (Show)

instance Formatable Constant where
    fmt (Constant n _) = Format n
        (\comment -> [ Heading H3 $ "constant " ++ n
                     , Paragraph comment
                     , CodeBlock $ show (Id n) ])

-- Parses a Scheme definition.
--
--  <definition> → (define <identifier> <expression>)
--
mkConstant :: Sexp -> Maybe Constant
mkConstant (List ((Id "define"):(Id name):expr:[])) =
    Just $ Constant name expr
mkConstant _ = Nothing
