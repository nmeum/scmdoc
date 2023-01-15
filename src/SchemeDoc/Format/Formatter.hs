module SchemeDoc.Format.Formatter
    (defFormatter, format)
where

import Data.Maybe (fromJust)
import Control.Applicative
import Text.Blaze.Html
import Text.Blaze.Internal (MarkupM(Append))

import SchemeDoc.Types
import SchemeDoc.Format.Util
import SchemeDoc.Format.Types
import SchemeDoc.Format.Procedure
import SchemeDoc.Format.Constant
import SchemeDoc.Format.Library

-- The default Formatter, can be extented via the Maybe applicative.
defFormatter :: Sexp -> Maybe Format
defFormatter sexp = fmt <$> mkConstant sexp
                <|> fmt <$> mkProcedure sexp

-- Format the given S-expression, if it is exported by the given library.
runFormat :: Library -> Formatter -> Maybe Sexp -> Maybe FormatF
runFormat _ _ Nothing = Just $ fromMkd
runFormat lib f (Just expr) = f expr >>= (\(Format i fn) -> if libExports lib i
                                                         then Just fn
                                                         else Nothing)

-- Format all documented S-expressions which are exported
-- by the given Scheme library using the given Formatter.
--
-- Returns pair of HTML for succesfully formatted S-expressions
-- and list of S-expressions which are documented but for which
-- no formatter was found.
--
-- TODO: Track parent for each formated S-expression. The parent
-- should be a section comment. Then generate a TOC from this
-- information using Data.List.groupBy.
format :: Library -> Formatter -> [Documented] -> (Html, [Sexp])
format lib formatFn = foldl (\(acc, failed) (comment, expr) ->
                                case runFormat lib formatFn expr of
                                    Just f  -> (Append acc (f comment), failed)
                                    Nothing -> (acc, failed ++ [fromJust expr])) ((toHtml ""), [])
