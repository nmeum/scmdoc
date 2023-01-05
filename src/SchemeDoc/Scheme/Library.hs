module SchemeDoc.Scheme.Library where

import SchemeDoc
import SchemeDoc.Format.Library
import SchemeDoc.Format.Formatter
import SchemeDoc.Scheme.Documented
import SchemeDoc.Documentation.AST

-- A documented Scheme library.
type DocLib = (String, Library)

-- Find all documented Scheme library declarations in a Scheme source.
findDocLibs :: [Sexp] -> Either SyntaxError [DocLib]
findDocLibs exprs = foldr fn (Right []) (findDocumented exprs)
    where
        fn (s, e@(List ((Id "define-library"):xs))) acc =
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
docFmt (libDesc, lib) decls = [Heading H1 $ libName lib, Paragraph libDesc]
    ++ format defFormatter decls
