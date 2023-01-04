module SchemeDoc.Format.Formatter
    (defFormatter, format)
where

import Control.Applicative

import SchemeDoc
import SchemeDoc.Scheme.Documented
import SchemeDoc.Documentation.AST
import SchemeDoc.Format.Types
import SchemeDoc.Format.Procedure
import SchemeDoc.Format.Constant

-- The default Formatter, can be extented via the Maybe applicative.
defFormatter :: Sexp -> Maybe Formatted
defFormatter sexp = fmt <$> mkConstant sexp
           <|> fmt <$> mkProcedure sexp

-- Format the given S-expression.
runFormat :: Formatter -> Sexp -> Formatted
runFormat f expr = case f expr of
    Just x  -> x
    Nothing -> fmt expr -- TODO: Emit warning

-- Convert a formatted item and a comment into a markup block.
formatItem :: String -> Formatted -> [Block String]
formatItem comment Formatted{objDesc=desc, objName=n, objExpr=e} = [
    Heading H2 $ desc ++ ": " ++ n,
    Paragraph comment
    , CodeBlock $ show e
    ]

-- Format all documented S-expressions using the given formatter.
format :: Formatter -> [Documented] -> [Block String]
format formatFn = concat . map (\(Documented comment expr) ->
                                    formatItem comment (runFormat formatFn expr))
