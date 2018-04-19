module Test where

import Data.Aeson as Aeson (ToJSON(..), encode)
import Text.ParserCombinators.Parsec
import Data.ByteString.Lazy.Char8 as DBLC8 (ByteString, putStrLn, pack, unpack)
import Data.Text as T (Text, pack)
import Data.Either
import Parse

-- Run the address book parse on a test input
test,t :: GenParser Char () a -> String -> Either ParseError a
test p testCase = parse p "parse Failure" testCase
t = test

jsonTest :: (ToJSON a) => GenParser Char () a -> String -> String
jsonTest p s = either show (DBLC8.unpack . encode) (test p s)

