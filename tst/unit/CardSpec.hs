module CardSpec where

import Test.Hspec
import Test
import Parse
import Data.Aeson as Aeson (ToJSON(..), encode)
import Data.Text as T (unpack, pack)
import Data.ByteString.Lazy.Char8 as DBLC8 (pack)
import Text.ParserCombinators.Parsec

gronk :: GenParser Char st String
gronk = string "gronk"

spec :: Spec
spec = do
  describe "jsonTest" $ do
    it "X-ABUID:4709EC50-7594-4F67-85E1-6870DA65FCBA:ABPerson\n -> junk" $
      jsonTest attribute "a=b" `shouldBe` "{\"a\":\"b\"}"

    it "ab:\n" $
      jsonTest field "ab:\n" `shouldBe` "[{\"name\":\"\"}]"

    it "ab:c\n" $
      jsonTest field "ab:c\n" `shouldBe` "[{\"name\":\"c\"}]"

    it "N:;;;;\n" $
      jsonTest field "N:;;;;\n" `shouldBe` "[{\"name\":\"\"},{\"name\":\"\"},{\"name\":\"\"},{\"name\":\"\"},{\"name\":\"\"}]"

    it "PRODID:-//Apple Inc.//Mac OS X 10.13.4//EN\n" $
      jsonTest field "PRODID:-//Apple Inc.//Mac OS X 10.13.4//EN\n" `shouldBe` "[{\"name\":\"-//Apple Inc.//Mac OS X 10.13.4//EN\"}]"

    it ":kdkdk\n" $ -- Should fail (left "(unknown)")
      jsonTest field ":kdkdk\n" `shouldBe` "\"parse Failure\" (line 1, column 1):\nunexpected \":\""

    it "ORG:Macys;\n" $
      jsonTest field "ORG:Macys;\n" `shouldBe`  "[{\"name\":\"Macys\"},{\"name\":\"\"}]"

    it "\n" $ -- Should fail, empty line not allowed
      jsonTest field "\n" `shouldBe` "\"parse Failure\" (line 1, column 1):\nunexpected \"\\n\""

    it "TEL;type=CELL;type=VOICE;type=pref:15036451141\n" $
      jsonTest field "TEL;type=CELL;type=VOICE;type=pref:15036451141\n" `shouldBe` "[{\"type\":\"CELL\"},{\"type\":\"VOICE\"},{\"type\":\"pref\"},{\"name\":\"15036451141\"}]"
      
    it "ORG:Macys;\n -kdkdkdkd\n" $ -- Should fail because of '-' in the continuation
      jsonTest field "ORG:Macys;\n -kdkdkdkd\n" `shouldBe` "\"parse Failure\" (line 2, column 2):\nunexpected \"-\"\nexpecting letter or digit or \"\\n\""

    it "ORG:Macys--\n mcmcmcmc\n" $
      jsonTest field "ORG:Macys--\n mcmcmcmc\n" `shouldBe` "[{\"Continuation\":\"Macys--mcmcmcmc\"}]"

    it "\n mcmcmcmc\n" $
      jsonTest field "\n mcmcmcmc\n" `shouldBe` "\"parse Failure\" (line 1, column 1):\nunexpected \"\\n\""

    it "BEGIN:VCARD\nORG:Macys;\nEND:VCARD\n" $
      jsonTest card "BEGIN:VCARD\nORG:Macys;\nEND:VCARD\n" `shouldBe` "{\"fields\":[{\"ORG\":[{\"name\":\"Macys\"},{\"name\":\"\"}]}]}"

    it "BEGIN:VCARD\nORG:Macys;\nBDAY:2014-06-09\n continue\nNOTE:Has Immunization Record\nEND:VCARD" $
      jsonTest vcf "BEGIN:VCARD\nORG:Macys;\nBDAY:2014-06-09\n continue\nNOTE:Has Immunization Record\nEND:VCARD" `shouldBe` "junk" -- "{\"cards\":[{\"fields\":[{\"ORG\":[{\"name\":\"Macys\"},{\"name\":\"\"}]},{\"BDAY\":[{\"Continuation\":\"2014-06-09"}]},{\"NOTE\":[{\"name\":\"Has Immunization Record\"}]}]}]}"

    it "BEGIN:VCARD\nORG:Macys;\nEND:VCARD" $
      jsonTest vcf "BEGIN:VCARD\nORG:Macys;\nEND:VCARD" `shouldBe` "{\"card\":[{\"fields\":[{\"ORG\":[\"Macys\",\"\"]}]}]}"

    it "gronk" $ jsonTest (sepByEndBy gronk (char '.') (char ';')) "gronk.gronk" `shouldBe` "[\"gronk\"]"

    it "gronk" $ jsonTest (sepByEndBy gronk (char '.') (char ';')) "gronk.gronk;" `shouldBe` "[\"gronk\"]"

    it "gronk" $ jsonTest (sepByEndBy gronk (char '.') (char ';')) "gronk" `shouldBe` "[\"gronk\"]"

    it "gronk" $ jsonTest (sepByEndBy gronk (char '.') (char ';')) "gronk;" `shouldBe` "[\"gronk\"]"
