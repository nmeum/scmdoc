module SchemeDoc.Scheme.Library
    (Library, findLibrary)
where

import SchemeDoc

-- An R⁷RS Scheme library as defined in Section 5.6 of the standard.
data Library = MkLibrary { name :: String
                         --, imports :: [String]
                         --, exports :: [String]
                         , body :: [Sexp] }
    deriving (Show)

-- Check if the given s-expression constitutes an R⁷RS library declaration.
findLibrary' :: Sexp -> Maybe Library
findLibrary' (List ((Id "define-library"):(Id lname):xs)) =
    Just $ MkLibrary lname xs
findLibrary' _ = Nothing

-- Find a library declaration in a Scheme source.
-- This function only considers top-level declarations.
findLibrary :: [Sexp] -> Maybe Library
findLibrary (expr:xs) =
    case findLibrary' expr of
        Just lib -> Just lib
        Nothing  -> findLibrary xs
findLibrary [] = Nothing
-- TODO: A source file may have multiple library declarations.
