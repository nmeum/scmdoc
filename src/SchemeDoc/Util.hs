module SchemeDoc.Util where

import qualified Data.Text as T

isSpace :: Char -> Bool
isSpace = (==) ' '

-- Remove all leading ASCII space characters.
ltrim :: T.Text -> T.Text
ltrim = T.dropWhile isSpace
