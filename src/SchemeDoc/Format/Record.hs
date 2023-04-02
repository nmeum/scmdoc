{-# LANGUAGE OverloadedStrings #-}

-- | This module implements comment-preserving pseudo macro expansion
-- for record type definitions. All relevant record type comments are
-- expanded to dummy procedure definitions, allowing for them to be
-- formatter with the 'SchemeDoc.Formatter.Procedure' formatter later.
module SchemeDoc.Format.Record (expand) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import SchemeDoc.Format.Util
import SchemeDoc.Types

-- Return a new list where the element at the given index has
-- been modified according to the given function. If the index
-- is out of range, or if the functions returns 'Nothing', then
-- 'Nothing' is returned. Indices start at zero.
modifyAt :: (a -> Maybe a) -> [a] -> Int -> Maybe [a]
modifyAt f ls i
    | i < 0 = Nothing
    | otherwise = go i ls
  where
    go 0 (x : xs) = (: xs) <$> f x
    go n (x : xs) = (x :) <$> go (n - 1) xs
    go 0 [] = Just []
    go _ _ = Nothing

objIdx :: Int -> [Sexp] -> Maybe Int
objIdx n' = go n' 0
  where
    go :: Int -> Int -> [Sexp] -> Maybe Int
    go n i ((DocComment _) : xs) = go n (i + 1) xs
    go n i (_ : xs) = if n == 0 then Just i else go (n - 1) (i + 1) xs
    go _ _ _ = Nothing

mkProc :: [Sexp] -> T.Text -> Sexp
mkProc params name = List [Id "define", List $ Id name : params, Id "_"]

------------------------------------------------------------------------

-- Expand a record type constructor into a dummy procedure definition.
--
-- > <constructor> → (<identifier> <field name>*)
expandCons :: [Sexp] -> Maybe [Sexp]
expandCons xs = do
    idx <- objIdx 0 xs
    modifyAt expandCons' xs idx
  where
    expandCons' (List (Id consName : fields)) = Just $ mkProc fields consName
    expandCons' _ = Nothing

-- Expand a record type predicate into a dummy procedure definition.
expandPred :: [Sexp] -> Maybe [Sexp]
expandPred xs = objIdx 1 xs >>= modifyAt (onId $ mkProc [Id "obj"]) xs

-- Expand a record field spec into a dummy procedure definition.
--
-- > <field spec> → (<field name> <accessor>)
-- >    | (<field name> <accessor> <mutator>)
expandField :: T.Text -> Sexp -> Maybe Sexp
expandField typeName (List xs) = do
    -- Expand accessor procedure (must be present).
    idx <- objIdx 1 xs
    expanded <- modifyAt (onId $ mkProc [Id typeName]) xs idx

    pure (List $ Id "begin" : fromMaybe expanded (mutator expanded))
  where
    -- Optionally expand mutator procedure.
    mutator lst = do
        idx <- objIdx 2 lst
        modifyAt (onId $ mkProc [Id typeName, Id "new-value"]) lst idx
expandField _ _ = Nothing

-- Expand multiple record type field specs into dummy procedures.
expandFields :: T.Text -> [Sexp] -> Maybe [Sexp]
expandFields typeName exprs = do
    -- Extract all fields.
    idx <- objIdx 2 exprs
    let (xs, fields) = splitAt idx exprs

    -- Expand all fields and append them to the existing list.
    (xs ++) <$> mapM (expandField typeName) fields

-- | Expand a record type definition into multiple dummy procedures
-- while preserving documentation comments. Thereby, allowing record
-- type constructors, predicates, and field accessors/mutators to be
-- documented as procedures.
--
-- > (define-record-type <identifier>
-- >    <constructor> <identifier> <field spec>*)
--
-- TODO: Use SyntaxError errors.
expand :: Sexp -> Maybe Sexp
expand (List (Id "define-record-type" : Id typeName : xs)) = do
    e <- expandCons xs >>= expandPred >>= expandFields (T.toLower typeName)
    Just $ List (Id "begin" : e)
expand _ = Nothing
