module SchemeDoc.Formatter where

import SchemeDoc.Scheme.Documented
import SchemeDoc.Documentation.AST

formatItem :: Documented -> [Block String]
formatItem (Documented comment sexp) = [
    Paragraph comment
    , CodeBlock $ show sexp
    ]

-- TODO: Pass encapsulating library to this function
-- TODO: Filter unexported functions
-- TODO: Perform a transformation form S-expression → {function, declaration, custom macro, …}
-- TODO: Allow passing custom formatter and custom transformations (e.g. for specific macros)
format :: [Documented] -> [Block String]
format = concat . map formatItem
