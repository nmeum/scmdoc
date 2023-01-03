module SchemeDoc.Documentation.Markdown (mkMarkdown) where

import Data.List (intercalate)
import SchemeDoc.Documentation.AST

headingPrefix :: HeadingLevel -> String
headingPrefix (H1) = "#"
headingPrefix (H2) = "##"
headingPrefix (H3) = "###"

toMarkdown :: Block String -> String
toMarkdown (Paragraph p) = p ++ "\n"
toMarkdown (CodeBlock c) = intercalate "\n" $ map ((++) "\t") $ lines c
toMarkdown (Listing lst) = intercalate "\n" $ map ((++) "* ") lst
toMarkdown (Heading l s) = (headingPrefix l) ++ s

mkMarkdown :: [Block String] -> String
mkMarkdown = intercalate "\n" . map toMarkdown
