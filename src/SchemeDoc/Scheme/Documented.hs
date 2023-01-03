module SchemeDoc.Scheme.Documented (findDocumented) where

import Data.List (partition)
import SchemeDoc

-- TODO: This implementation is entirly awful and needs to be refactored.
--   * Reduce the complexity of findDocumented
--   * Consider joining multi-line DocComment in the Parser already
--   * …

-- A documented S-expression, i.e. an expression which is preceded by a DocComment.
data Documented = Documented String Sexp
    deriving (Show)

isDocComment :: Sexp -> Bool
isDocComment (DocComment _) = True
isDocComment _ = False

findDocumented' :: [Sexp] -> Sexp -> [Sexp]
findDocumented' ((DocComment s'):xs) (DocComment s) = (DocComment $ s ++ s') : xs
findDocumented' acc c@(DocComment _) = c : acc
findDocumented' acc@((DocComment _):_) expr = expr : acc
findDocumented' acc _ = acc

findDocumented :: [Sexp] -> [Documented]
findDocumented src = map (\(DocComment s, e) -> Documented s e) (zip docs exprs)
    where
        filtered = walk findDocumented' [] src
        (docs, exprs) = partition isDocComment $ reverse filtered
