module SchemeDoc
    (DocLib, findDocLibs, docDecls, docFmt)
where

import SchemeDoc.Types
import SchemeDoc.Error
import SchemeDoc.Format.Types
import SchemeDoc.Format.Library
import SchemeDoc.Format.Formatter
import SchemeDoc.Output

-- A documented Scheme library.
type DocLib = (String, Library)

-- Find all documented Scheme library declarations in a Scheme source.
findDocLibs :: [Sexp] -> Either SyntaxError [DocLib]
findDocLibs exprs = foldr fn (Right []) (findDocumented exprs)
    where
        fn (s, e@(List ((Id "define-library"):_))) acc =
            case mkLibrary e of
                Right lib -> fmap ((:) (s, lib)) acc
                Left err  -> Left err
        fn _ acc = acc

-- Find all documented declarations of a library.
-- Performs file system accesses to expand includes.
docDecls :: DocLib -> IO [Documented]
docDecls (_, lib) = libExpand lib >>= pure . findDocumented

-- Expand a documented library wrt. its declarations.
docFmt :: DocLib -> [Documented] -> [Block String]
docFmt (libDesc, lib) decls =
    ((fmt lib) libDesc) ++ format defFormatter decls

-- Filter all non-documented S-expressions.
filterDocs :: [Sexp] -> [Sexp]
filterDocs = snd . walk filterDocs' (False, [])
    where
        filterDocs' :: (Bool, [Sexp]) -> Sexp -> (Bool, [Sexp])
        filterDocs' (False, acc) c@(DocComment _) = (True, acc ++ [c])
        filterDocs' (True, acc) expr = (False, acc ++ [expr])
        filterDocs' b _ = b

findDocumented :: [Sexp] -> [Documented]
findDocumented = toPairLst . filterDocs
    where
        toPairLst :: [Sexp] -> [(Documented)]
        toPairLst [] = []
        toPairLst ((DocComment s):expr:xs) = (s, expr) : toPairLst xs
        toPairLst _ = error "unreachable"
