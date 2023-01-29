module SchemeDoc.Util where

import Data.Char (isSpace)
import qualified Data.Text as T

-- Remove all leading whitespaces.
ltrim :: T.Text -> T.Text
ltrim = T.dropWhile isSpace

-- Remove all trailing whitespaces.
rtrim :: T.Text -> T.Text
rtrim = T.dropWhileEnd isSpace

-- Remove both leading and trailing whitespaces.
trim :: T.Text -> T.Text
trim = rtrim . ltrim
