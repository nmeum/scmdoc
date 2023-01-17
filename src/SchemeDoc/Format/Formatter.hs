module SchemeDoc.Format.Formatter
    (defFormatter, format)
where

import Control.Applicative
import Text.Blaze.Html

import SchemeDoc.Types
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
--
-- Returns pair of HTML for succesfully formatted S-expressions
-- and list of S-expressions which are documented but for which
-- no formatter was found.
format :: Library -> Formatter -> [Documented] -> (Html, [Sexp])
format lib formatFn = foldl (\(acc, failed) (comment, expr) ->
                                case runFormat lib formatFn expr of
                                    Just f  -> (acc >> (f comment), failed)
                                    Nothing -> (acc, failed ++ [expr])) ((toHtml ""), [])
