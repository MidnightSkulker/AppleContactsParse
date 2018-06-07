-- Some testing of the Text.Regex Library
module RE where

import           Text.Regex

re1 :: Regex
re1 = mkRegex "[0-9]+"
re2 :: Regex
re2 = mkRegexWithOpts "[0-9]+" True True
re3 :: Regex
re3 = mkRegex "9"

m1, m2, m3, m4 :: Maybe [String]
m1 = matchRegex re1 "949494"
m2 = matchRegex re2 "949494"
m3 = matchRegex re1 "949494 xxx 8484"
m4 = matchRegex re3 "949494 xxx 8484"

mall1 :: Maybe (String, String, String, [String])
mall1 = matchRegexAll re1 "949494"

-- For matching the multipl entry items in the address book
itemRE :: Regex
itemRE = mkRegex "^item[0-9]+"
numberRE :: Regex
numberRE = mkRegex "[0-9]+"
item1MatchAll, item2MatchAll :: Maybe (String, String, String, [String])
item1MatchAll = matchRegexAll itemRE "item123.X-ABLabel"
item2MatchAll = matchRegexAll itemRE "item1.EMAIL"
item1Match, item2Match :: Maybe [String]
item1Match = matchRegex itemRE "item123.X-ABLabel"
item2Match = matchRegex itemRE "item1.EMAIL"

isItem :: String -> Maybe (String, String, String)
isItem s =
  let mMatch = matchRegexAll itemRE s
  in case mMatch of
       Just (_before, match, after, _subs) ->
         case matchRegexAll numberRE match of
           Just (_, n, _, _) -> Just (match, after, n)
           Nothing -> Nothing
       Nothing -> Nothing
