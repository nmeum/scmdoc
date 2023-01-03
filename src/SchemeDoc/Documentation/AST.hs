{-# LANGUAGE DeriveFoldable #-}
module SchemeDoc.Documentation.AST where

data HeadingLevel = H1 | H2 | H3

data Block a = Paragraph a
             | CodeBlock a
             | Listing [a]
             | Heading HeadingLevel a

instance Functor Block where
    fmap f (Paragraph p) = Paragraph $ f p
    fmap f (CodeBlock a) = CodeBlock $ f a
    fmap f (Listing a)   = Listing $ map f a
    fmap f (Heading n a) = Heading n $ f a
