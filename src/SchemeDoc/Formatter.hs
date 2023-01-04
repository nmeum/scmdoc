{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
module SchemeDoc.Formatter where

import Control.Applicative

import SchemeDoc
import SchemeDoc.Scheme.Documented
import SchemeDoc.Documentation.AST

data Formatted = Formatted { objDesc :: String -- e.g. procedure
                           , objName :: String -- e.g. my-func
                           , objExpr :: Sexp }

class Formatable a where
    fmt :: a -> Formatted

instance Formatable Sexp where
    fmt expr = Formatted "expression" "expr" expr

------------------------------------------------------------------------

data Procedure = Procedure { name   :: String
                           , params :: [String]
                           , body   :: [Sexp] }
    deriving (Show)

instance Formatable Procedure where
    fmt (Procedure{name=n, params=p}) =
        Formatted "procedure" n $ List ([Id n] ++ map Id p)

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

------------------------------------------------------------------------

data Constant = Constant { consName  :: String
                         , consValue :: Sexp }

instance Formatable Constant where
    fmt (Constant n _) = Formatted "constant" n $ Id n

mkConstant :: Sexp -> Maybe Constant
mkConstant _ = Nothing

------------------------------------------------------------------------

defFormatter :: Sexp -> Maybe Formatted
defFormatter sexp = fmt <$> mkConstant sexp
           <|> fmt <$> mkProcedure sexp

type Formatter = Sexp -> Maybe Formatted

runFormat :: Formatter -> Sexp -> Formatted
runFormat f expr = case f expr of
    Just x  -> x
    Nothing -> fmt expr -- TODO: Emit warning

formatItem :: String -> Formatted -> [Block String]
formatItem comment Formatted{objDesc=desc, objName=n, objExpr=e} = [
    Heading H2 $ desc ++ ": " ++ n,
    Paragraph comment
    , CodeBlock $ show e
    ]

format' :: Formatter -> [Documented] -> [Block String]
format' formatFn = concat . map (\(Documented comment expr) ->
                                    formatItem comment (runFormat formatFn expr))

-- TODO: Pass encapsulating library to this function
-- TODO: Filter unexported functions
-- TODO: Perform a transformation form S-expression → {function, declaration, custom macro, …}
-- TODO: Allow passing custom formatter and custom transformations (e.g. for specific macros)
format :: [Documented] -> [Block String]
format = format' defFormatter
