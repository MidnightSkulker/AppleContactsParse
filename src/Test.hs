{-# LANGUAGE ScopedTypeVariables #-}
module Test where

import Data.Aeson as Aeson (ToJSON(..), encode)
import Text.ParserCombinators.Parsec
import Data.ByteString.Lazy.Char8 as DBLC8 (ByteString, putStrLn, pack, unpack)
import Data.Aeson as Aeson (ToJSON(..), object, pairs, (.=), encode, Value(..), KeyValue(..), toJSONList)
import Data.Text as T (Text, pack)
import Data.Either ()
import Data.Either.Utils (fromRight)
import Parse

-- Run the address book parse on a test input
test,t :: GenParser Char () a -> String -> Either ParseError a
test p testCase = parse p "parse Failure" testCase
t = test

jsonTest :: (ToJSON a) => GenParser Char () a -> String -> String
jsonTest p s = either show (DBLC8.unpack . encode) (test p s)

parsTest :: (ToJSON a) => GenParser Char () a -> String -> a
parsTest p s = fromRight (test p s)

t1 :: String = jsonTest vcf "BEGIN:VCARD\nORG:Macys\nEND:VCARD\nBEGIN:VCARD\nORG:Target\nEND:VCARD"

t2 :: String = jsonTest vcf "BEGIN:VCARD\nORG:Macys\nEND:VCARD"

-- Some Test values


aa :: Attribute = ComplexAttribute "a" "1"
av :: Value = oneField aa
ao :: Value = object [T.pack "a" .= (1 :: Integer)]
ba :: Attribute = ComplexAttribute "b" "2"
bv :: Value = oneField ba
bo :: Value = object [T.pack "b" .= (2 :: Integer)]
ca :: Attribute = ComplexAttribute "c" "3"
attrs1 :: [Attribute] = [aa, ba, ca]

f1 :: Field = Field { pangalan = "f1", attributes = attrs1 }
f2 :: Field = parsTest field "URL;type=WORK;type=pref:mychart.tpcllp.com/MyChart/"
f3 :: Field = Field { pangalan = "hhh", attributes = [aa, ba, ca] }
-- Because aeson uses a hashmap, it will only record one attribute with
-- the same key.
f4 :: Field = Field { pangalan = "iii", attributes = [aa, aa, aa] }

f5 :: Field = parsTest field "ab:\n"
f5v :: Value =  toJSON f5
f5e :: ByteString = encode f5v
abo :: Value = object [T.pack "a" .= (1 :: Integer), T.pack "b" .= (2 :: Integer)]
abv1 :: [Value] = [av, bv]
abv2 :: Value = object [T.pack "a" .= "1", T.pack "b" .= "2"]
abv4 :: Value = object (map fromPair [("a", "1"), ("b", "2"), ("c","")])

attrs2 :: Value = mkObjectFromPairable attributeToPair [aa, ba, ca]

card1 :: Card = parsTest card "BEGIN:VCARD\nORG:Macys;\na:1;type=x\nEND:VCARD\n"

