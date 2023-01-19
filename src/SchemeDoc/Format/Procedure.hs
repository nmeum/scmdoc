{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module SchemeDoc.Format.Procedure where

import qualified Data.Text as T

import SchemeDoc.Types
import SchemeDoc.Format.Types
import SchemeDoc.Format.Util

data Procedure = Procedure { name   :: T.Text
                           , params :: [T.Text]
                           , body   :: [Sexp] }
    deriving (Show)

instance Formatable Procedure where
    fmt (Procedure{name=n, params=p}) = Format n
        (\comment -> do
                        component "procedure" n
                        fromMkd comment
                        htmlSexp $ List ([Id n] ++ map Id p))

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
