module Date ( comparableDate ) where

import Data.List (break)

-- Convert a date from "03/23/2016" format into
-- "2016-03-26" format, so that dates can be compared by the
-- unix command jq

split :: Char -> String -> [String]
split delim str =
  let (a, db) = break (== delim) str
      _d:b = db
  in if db == ""
     then [a]
     else a : split delim b

comparableDate :: String -> String
comparableDate s =
  let monthDayYear = split '/' s
  in case monthDayYear of
       [month, day, year] -> year ++ "-" ++ month ++ "-" ++ day
       _ -> s
