module SchemeDoc.Format.Formatter
    (defFormatter, format)
where

import Control.Applicative

import SchemeDoc.Types
import SchemeDoc.Output
import SchemeDoc.Format.Types
import SchemeDoc.Format.Procedure
import SchemeDoc.Format.Constant

-- The default Formatter, can be extented via the Maybe applicative.
defFormatter :: Sexp -> Maybe FormatS
defFormatter sexp = fmt <$> mkConstant sexp
           <|> fmt <$> mkProcedure sexp

-- Format the given S-expression.
runFormat :: Formatter -> Sexp -> FormatS
runFormat f expr = case f expr of
    Just x  -> x
    Nothing -> fmt expr -- TODO: Emit warning

-- Format all documented S-expressions using the given formatter.
format :: Formatter -> [Documented] -> [Block String]
format formatFn = concat . map (\(comment, expr) ->
                                    let f = runFormat formatFn expr in
                                        f comment)
