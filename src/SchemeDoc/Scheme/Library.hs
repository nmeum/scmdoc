module SchemeDoc.Scheme.Library
    (Library, findLibrary)
where

import Data.List (intercalate)
import SchemeDoc

newtype LibraryName = MkLibName [String]
instance Show LibraryName where
    show (MkLibName lst) = intercalate " " lst

mkLibName :: Sexp -> Maybe LibraryName
mkLibName (List exprs) = MkLibName <$>
    foldr (\x acc -> case x of
        Id  ident -> fmap ((:) ident) acc
        -- TODO: Only allow exact non-negative integers.
        Number n  -> fmap ((:) (show n :: String)) acc
        _         -> Nothing) (Just []) exprs
mkLibName _ = Nothing
-- TODO: Actual error messages for invalid names (i.e. use Either).

-- An R⁷RS Scheme library as defined in Section 5.6 of the standard.
data Library = MkLibrary { name :: LibraryName
                         --, imports :: [String]
                         --, exports :: [String]
                         , body :: [Sexp] }
    deriving (Show)

-- Check if the given s-expression constitutes an R⁷RS library declaration.
findLibrary' :: Sexp -> Maybe Library
findLibrary' (List ((Id "define-library"):x:xs)) =
    fmap (\n -> MkLibrary n xs) $ mkLibName x
findLibrary' _ = Nothing

-- Find library declarations in a Scheme source.
-- XXX: This function only considers top-level declarations.
findLibrary :: [Sexp] -> [Library]
findLibrary = foldl (\acc x -> case findLibrary' x of
                        Just lib -> lib : acc
                        Nothing  -> acc) []
