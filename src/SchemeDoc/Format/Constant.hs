module SchemeDoc.Format.Constant where

import SchemeDoc.Types
import SchemeDoc.Output
import SchemeDoc.Format.Types

data Constant = Constant { consName  :: String
                         , consValue :: Sexp }
    deriving (Show)

instance Formatable Constant where
    sid (Constant n _) = n
    fmt (Constant n _) = (\comment -> [ Heading H2 $ "Constant " ++ n
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
