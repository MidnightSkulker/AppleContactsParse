{-# LANGUAGE ScopedTypeVariables #-}
module Test where

import Data.Aeson as Aeson (ToJSON(..))
import Text.ParserCombinators.Parsec (parse, GenParser, ParseError)
import Data.ByteString.Lazy.Char8 as DBLC8 (ByteString, unpack)
import Data.Aeson as Aeson (object, (.=), encode, Value(..), KeyValue(..))
import Data.Text as T (pack)
import Data.Either ()
import Data.Either.Utils (fromRight)
import Data.Maybe
import Parse

-- Run the address book parse on a test input
test,t :: GenParser Char () a -> String -> Either ParseError a
test p testCase = parse p "parse Failure" testCase
t = test

jsonTest :: (ToJSON a) => GenParser Char () a -> String -> String
jsonTest p s = either show (DBLC8.unpack . encode) (test p s)

parsTest :: (ToJSON a) => GenParser Char () a -> String -> a
parsTest p s = fromRight (test p s)

t1 :: String
t1 = jsonTest (vcf []) "BEGIN:VCARD\nORG:Macys\nEND:VCARD\nBEGIN:VCARD\nORG:Target\nEND:VCARD"

t2 :: String
t2 = jsonTest (vcf []) "BEGIN:VCARD\nORG:Macys\nEND:VCARD"

t3 :: String
t3 = jsonTest (vcf []) "BEGIN:VCARD\nORG:Kasalukuyang Estudyante\nEND:VCARD\nBEGIN:VCARD\nORG:Kasalukuyang Estudyante\nEND:VCARD"

-- Some Test values
aa :: Attribute
aa = ComplexAttribute "a" "1"
ahv :: Bool
ahv = attrHasValue "a" Nothing aa
ao :: Value
ao = object [T.pack "a" .= (1 :: Integer)]
ba :: Attribute
ba = ComplexAttribute "b" "2"
bo :: Value
bo = object [T.pack "b" .= (2 :: Integer)]
ca :: Attribute
ca = ComplexAttribute "c" "3"
attrs1 :: [Attribute]
attrs1 = [aa, ba, ca]

f1 :: Field
f1 = Field { pangalan = "f1", attributes = attrs1 }
f2 :: Field
f2 = parsTest (field []) "URL;type=WORK;type=pref:mychart.tpcllp.com/MyChart/"
f3 :: Field
f3 = Field { pangalan = "hhh", attributes = [aa, ba, ca] }
-- Because aeson uses a hashmap, it will only record one attribute with
-- the same key.
f4 :: Field
f4 = Field { pangalan = "iii", attributes = [aa, aa, aa] }

f5 :: Field
f5 = parsTest (field []) "ab:\n"
f5v :: Value
f5v = toJSON f5
f5e :: ByteString
f5e = encode f5v
f6 :: Field
f6 = parsTest (field []) "ORG:Macys;\n"
f6v :: Value
f6v = toJSON f6
f6e :: ByteString
f6e = encode f6v
f7 :: Field
f7 = parsTest (field []) "ORG:Macys-\n kdkdkdkd\n"
f7v :: Value
f7v = toJSON f7
f7e :: ByteString
f7e = encode f7v
abo :: Value
abo = object [T.pack "a" .= (1 :: Integer), T.pack "b" .= (2 :: Integer)]
abv2 :: Value
abv2 = object [T.pack "a" .= "1", T.pack "b" .= "2"]
abv4 :: Value
abv4 = object (map fromPair [("a", "1"), ("b", "2"), ("c","")])

attrs2 :: Value
attrs2 = mkObjectFromPairable attributeToPair [aa, ba, ca]

card1 :: Card
card1 = parsTest (card []) "BEGIN:VCARD\nORG:Macys;\na:1;type=x\nEND:VCARD\n"
