module SchemeDoc.Scheme.Documented where

import SchemeDoc

type Documented = (String, Sexp)

-- Fold function for a left fold which returns all all documented
-- S-expressions. Operates on a pair to track both the accumulated
-- S-expressions and whether the last traversed S-expression was a
-- comment. The latter condition is indicated by the bool in the tuple.
filterDocs' :: (Bool, [Sexp]) -> Sexp -> (Bool, [Sexp])
filterDocs' (False, acc) c@(DocComment _) = (True, acc ++ [c])
filterDocs' (True, acc) expr = (False, acc ++ [expr])
filterDocs' b _ = b

-- Filter all non-documented S-expressions.
filterDocs :: [Sexp] -> [Sexp]
filterDocs = snd . walk filterDocs' (False, [])

findDocumented :: [Sexp] -> [Documented]
findDocumented = toPairLst . filterDocs
    where
        toPairLst :: [Sexp] -> [(Documented)]
        toPairLst [] = []
        toPairLst ((DocComment s):expr:xs) = (s, expr) : toPairLst xs
        toPairLst _ = error "unreachable"
