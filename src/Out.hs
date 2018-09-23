module Out ( niceList ) where

import Data.List (intercalate)

-- Print out a list one element per line
niceList :: (Show a) => [a] -> String
niceList as = intercalate "\n" (map show as)
