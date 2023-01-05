{-# LANGUAGE LambdaCase #-}
module SchemeDoc.Format.Procedure where

import SchemeDoc.Types
import SchemeDoc.Output
import SchemeDoc.Format.Types

data Procedure = Procedure { name   :: String
                           , params :: [String]
                           , body   :: [Sexp] }
    deriving (Show)

instance Formatable Procedure where
    sid (Procedure{name=n}) = n
    fmt (Procedure{name=n, params=p}) =
        (\comment -> [ Heading H2 $ "Procedure " ++ n
                     , Paragraph comment
                     , CodeBlock $ show $ List ([Id n] ++ map Id p)])

-- Parses a Scheme procedure definition.
--
--  <procedure> → (define (<identifier> <def formals>) <body>)
--
-- where
--
--  <def formals> → <identifier>* | <identifier>* . <identifier>
--
-- TODO: Support the latter def formal rule.
mkProcedure :: Sexp -> Maybe Procedure
mkProcedure (List ((Id "define"):(List ((Id defid):arglst)):bodylst)) =
    ((flip $ Procedure defid) bodylst) <$>
        mapM (\case
            Id arg -> Just arg
            _      -> Nothing) arglst
mkProcedure _ = Nothing
