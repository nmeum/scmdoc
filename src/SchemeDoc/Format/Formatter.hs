module SchemeDoc.Format.Formatter
    (defFormatter, format)
where

import Control.Applicative

import SchemeDoc.Types
import SchemeDoc.Output
import SchemeDoc.Format.Types
import SchemeDoc.Format.Procedure
import SchemeDoc.Format.Constant
import SchemeDoc.Format.Library

-- The default Formatter, can be extented via the Maybe applicative.
defFormatter :: Sexp -> Maybe Format
defFormatter sexp = fmt <$> mkConstant sexp
                <|> fmt <$> mkProcedure sexp

-- Format the given S-expression, if it is exported by the given library.
runFormat :: Library -> Formatter -> Sexp -> Maybe FormatF
runFormat lib f expr = f expr >>= (\(Format i fn) -> if libExports lib i
                                                         then Just fn
                                                         else Nothing)

-- Format all documented S-expressions which are exported
-- by the given Scheme library using the given Formatter.
format :: Library -> Formatter -> [Documented] -> [Block String]
format lib formatFn = foldl (\acc (comment, expr) ->
                                case runFormat lib formatFn expr of
                                    Just f  -> acc ++ (f comment)
                                    Nothing -> acc) []
