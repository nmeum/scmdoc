module SchemeDoc.Output
    (HeadingLevel(..), Block(..), mkMarkdown)
where

import Data.List (intercalate)

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

------------------------------------------------------------------------

headingPrefix :: HeadingLevel -> String
headingPrefix (H1) = "#"
headingPrefix (H2) = "##"
headingPrefix (H3) = "###"

toMarkdown :: Block String -> String
toMarkdown (Paragraph p) = p ++ "\n"
toMarkdown (CodeBlock c) = intercalate "\n" $ map ((++) "\t") $ lines c
toMarkdown (Listing lst) = (intercalate "\n" $ map ((++) "* ") lst) ++ "\n"
toMarkdown (Heading l s) = (headingPrefix l) ++ " " ++ s

mkMarkdown :: [Block String] -> String
mkMarkdown = intercalate "\n" . map toMarkdown
