-- Some testing of the Text.Regex Library
module RE ( isItem, itemNumber, itemStructure ) where

import Text.Regex (Regex, mkRegex, matchRegexAll)
import Data.Maybe (fromJust)
import Data.Tuple.Utils (thd3)

-- For matching the multipl entry items in the address book
itemRE :: Regex
itemRE = mkRegex "^item[0-9]+."
numberRE :: Regex
numberRE = mkRegex "[0-9]+"

isItem :: String -> Maybe (String, String, String)
isItem s =
  let mMatch = matchRegexAll itemRE s
  in case mMatch of
       Just (_before, match, after, _subs) ->
         case matchRegexAll numberRE match of
           Just (_, n, _, _) -> Just (match, after, n)
           Nothing -> Nothing
       Nothing -> Nothing

-- When you are sure it is an item (otherwise this will make an exception)
itemStructure :: String -> (String, String, String)
itemStructure = fromJust . isItem
-- Get the number from the item structure
itemNumber :: String -> String
itemNumber = thd3 . fromJust . isItem
