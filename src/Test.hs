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

t1 :: String
t1 = jsonTest vcf "BEGIN:VCARD\nORG:Macys\nEND:VCARD\nBEGIN:VCARD\nORG:Target\nEND:VCARD"

t2 :: String
t2 = jsonTest vcf "BEGIN:VCARD\nORG:Macys\nEND:VCARD"

-- Some Test values


aa = ComplexAttribute "a" "1"
av = oneField aa
ao = object [T.pack "a" .= (1 :: Integer)]
ba = ComplexAttribute "b" "2"
bv = oneField ba
bo = object [T.pack "b" .= (2 :: Integer)]
ca = ComplexAttribute "c" "3"
f1 = Field { pangalan = "f1", attributes = [aa, ba, ca] }
abo = object [T.pack "a" .= (1 :: Integer), T.pack "b" .= (2 :: Integer)]
abv1 = [av, bv]
abv2 = object [T.pack "a" .= "1", T.pack "b" .= "2"]
abv4 = object (map fromPair [("a", "1"), ("b", "2"), ("c","")])

attrs1 = mkObjectFromAttributes toPair [aa, ba, ca]

card1 = parsTest card "BEGIN:VCARD\nORG:Macys;\na:1;type=x\nEND:VCARD\n"
