module Main where

import Data.Aeson as Aeson (ToJSON(..))
import Text.ParserCombinators.Parsec
import Data.ByteString.Lazy.Char8 as DBLC8 (ByteString)
import Parse
import Test

-- Test attribute parser
t1,t2,t3 :: Either ParseError Attribute
j1,j2,j3 :: String
t1 = test attribute "a=b"
j1 = jsonTest attribute "a=b"
t2 = test attribute "ab"
j2 = jsonTest attribute "ab"
t3 = test attribute "=b"
j3 = jsonTest attribute "=b"
-- Test field parser
t4,t5,t6,t7,t8,t9,t10,t11,t12 :: Either ParseError Field
j4,j5,j6,j7,j8,j9,j10,j11,j12 :: String
t4 = test field "X-ABUID:4709EC50-7594-4F67-85E1-6870DA65FCBA:ABPerson\n"
j4 = jsonTest field "X-ABUID:4709EC50-7594-4F67-85E1-6870DA65FCBA:ABPerson\n"
t5 = test field "ab:\n"
j5 = jsonTest field "ab:\n"
t6 = test field "ab:c\n"
j6 = jsonTest field "ab:c\n"
t7 = test field "N:;;;;\n"
j7 = jsonTest field "N:;;;;\n"
t8 = test field "PRODID:-//Apple Inc.//Mac OS X 10.13.4//EN\n"
j8 = jsonTest field "PRODID:-//Apple Inc.//Mac OS X 10.13.4//EN\n"
t9 = test field ":kdkdk\n" -- Should fail (left "(unknown)")
j9 = jsonTest field ":kdkdk\n" -- Should fail (left "(unknown)")
t10 = test field "ORG:Macys;\n"
j10 = jsonTest field "ORG:Macys;\n"
t11 = test field "\n" -- Should fail, empty line not allowed
j11 = jsonTest field "\n" -- Should fail, empty line not allowed
t12 = test field "TEL;type=CELL;type=VOICE;type=pref:15036451141\n"
j12 = jsonTest field "TEL;type=CELL;type=VOICE;type=pref:15036451141\n"
t21,t22,t23 :: Either ParseError Field
j21,j22,j23 :: String
t21 = test field "ORG:Macys;\n -kdkdkdkd\n" -- Should fail because of '-' in the continuation
j21 = jsonTest field "ORG:Macys;\n -kdkdkdkd\n" -- Should fail because of '-' in the continuation
t22 = test field "ORG:Macys--\n mcmcmcmc\n"
j22 = jsonTest field "ORG:Macys--\n mcmcmcmc\n"
t23 = test field "\n mcmcmcmc\n"
j23 = jsonTest field "\n mcmcmcmc\n"
t30 :: Either ParseError Card
j30 :: String
t30 = test card "BEGIN:VCARD\nORG:Macys;\nEND:VCARD\n"
j30 = jsonTest card "BEGIN:VCARD\nORG:Macys;\nEND:VCARD\n"
t40,t41 :: Either ParseError VCF
j40,j41 :: String
t40 = test vcfFile "BEGIN:VCARD\nORG:Macys;\nBDAY:2014-06-09\n continue\nNOTE:Has Immunization Record\nEND:VCARD"
j40 = jsonTest vcfFile "BEGIN:VCARD\nORG:Macys;\nBDAY:2014-06-09\n continue\nNOTE:Has Immunization Record\nEND:VCARD"
t41 = test vcfFile "BEGIN:VCARD\nORG:Macys;\nEND:VCARD"
j41 = jsonTest vcfFile "BEGIN:VCARD\nORG:Macys;\nEND:VCARD"

main :: IO ()
main = do
  arubala <- readFile "test/Arubala.test"
  parseTest card arubala

m = main
