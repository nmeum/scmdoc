{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module implement a 'Formatter' for Scheme procedure definitions.
--
-- For example:
--
-- > (define (my-proc x1 x2)
-- >    (* x1 x2))
--
module SchemeDoc.Format.Procedure where

import qualified Data.Text as T

import SchemeDoc.Types
import SchemeDoc.Format.Types
import SchemeDoc.Format.Util

-- | A R7RS Scheme procedure definition.
data Procedure = Procedure { name   :: T.Text   -- ^ Identifier, i.e. procedure name.
                           , params :: [T.Text] -- ^ Procedure parameters.
                           , body   :: [Sexp]   -- ^ Procedure body.
                           }
    deriving (Eq, Show)

instance Formatable Procedure where
    fmt (Procedure{name=n, params=p}) desc =
        Declaration n desc $ do
                        component "procedure" n
                        fromMkd desc
                        htmlSexp $ List ([Id n] ++ map Id p)

-- | Parses a Scheme procedure definition.
--
-- > <procedure> → (define (<identifier> <def formals>) <body>)
--
-- where
--
-- > <def formals> → <identifier>* | <identifier>* . <identifier>
--
mkProcedure :: Sexp -> Maybe Procedure
mkProcedure (List ((Id "define"):(List ((Id defid):arglst)):bodylst)) =
    ((flip $ Procedure defid) bodylst) <$>
        mapM (\case
            Id arg -> Just arg
            _      -> Nothing) arglst
mkProcedure _ = Nothing
