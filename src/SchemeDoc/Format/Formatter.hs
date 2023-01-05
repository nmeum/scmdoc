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
defFormatter :: Sexp -> Maybe (String, FormatS)
defFormatter sexp = (mkConstant sexp >>= mkPair)
           <|> (mkProcedure sexp >>= mkPair)
    where
        mkPair :: Formatable a => a -> Maybe (String, FormatS)
        mkPair x = Just $ (sid x, fmt x)

-- Format the given S-expression, if it is exported by the given library.
runFormat :: Library -> Formatter -> Sexp -> Maybe FormatS
runFormat lib f expr = case f expr of
    Just (n, x) -> if libExports lib n
                     then Just x
                     else Nothing
    Nothing     -> Just $ fmt expr -- TODO: Emit warning

-- Format all documented S-expressions which are exported
-- by the given Scheme library using the given Formatter.
format :: Library -> Formatter -> [Documented] -> [Block String]
format lib formatFn = foldl (\acc (comment, expr) ->
                                case runFormat lib formatFn expr of
                                    Just f  -> (f comment) ++ acc
                                    Nothing -> acc) []
