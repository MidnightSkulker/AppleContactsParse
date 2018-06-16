{-# LANGUAGE OverloadedStrings #-}
module CardSpec where

import Test.Hspec
import Test
import Parse
import Text.ParserCombinators.Parsec

gronk :: GenParser Char st String
gronk = string "gronk"

spec :: Spec
spec = do
  describe "jsonTest" $ do
    it "X-ABUID:4709EC50-7594-4F67-85E1-6870DsA65FCBA:ABPerson\n -> junk" $
      jsonTest attribute "a=b" `shouldBe` "{\"a\":\"b\"}"

    it "ab:\n" $
      jsonTest (field []) "ab:\n" `shouldBe` "{\"ab\":\"\"}"

    it "ab:c\n" $
      jsonTest (field []) "ab:c\n" `shouldBe` "{\"ab\":\"c\"}"

    it "N:;;;;\n" $
      jsonTest (field []) "N:;;;;\n" `shouldBe` "{\"N\":[\"\",\"\",\"\",\"\",\"\"]}"

    it "PRODID:-//Apple Inc.//Mac OS X 10.13.4//EN\n" $
      jsonTest (field []) "PRODID:-//Apple Inc.//Mac OS X 10.13.4//EN\n" `shouldBe` "{\"PRODID\":\"-//Apple Inc.//Mac OS X 10.13.4//EN\"}"

    it ":kdkdk\n" $ -- Should fail (left "(unknown)")
      jsonTest (field []) ":kdkdk\n" `shouldBe` "\"parse Failure\" (line 1, column 1):\nunexpected \":\"\nexpecting \"item\" or \"URL;\""

    it "ORG:Macys;\n" $
      jsonTest (field []) "ORG:Macys;\n" `shouldBe` "{\"ORG\":[\"Macys\",\"\"]}"

    it "\n" $ -- Should fail, empty line not allowed
      jsonTest (field []) "\n" `shouldBe` "\"parse Failure\" (line 1, column 1):\nunexpected \"\\n\"\nexpecting \"item\" or \"URL;\""

    it "TEL;type=CELL;type=VOICE;type=pref:15036451141\n" $
      jsonTest (field []) "TEL;type=CELL;type=VOICE;type=pref:15036451141\n" `shouldBe` "{\"TEL\":[{\"type\":\"CELL\"},{\"type\":\"VOICE\"},{\"type\":\"pref\"},\"15036451141\"]}"

    it "ORG:Macys-;\n kdkdkdkd\n" $
      jsonTest (field []) "ORG:Macys;\n -kdkdkdkd\n" `shouldBe` "{\"ORG\":[\"Macys\",\"-kdkdkdkd\"]}"

    it "ORG:Macys--\n mcmcmcmc\n" $
      jsonTest (field []) "ORG:Macys--\n mcmcmcmc\n" `shouldBe` "{\"ORG\":\"Macys--mcmcmcmc\"}"

    it "\n mcmcmcmc\n" $
      jsonTest (field []) "\n mcmcmcmc\n" `shouldBe` "\"parse Failure\" (line 1, column 1):\nunexpected \"\\n\"\nexpecting \"item\" or \"URL;\""

    it "BEGIN:VCARD\nORG:Macys;\nEND:VCARD\n" $
      jsonTest (card []) "BEGIN:VCARD\nORG:Macys;\nEND:VCARD\n" `shouldBe` "{\"fields\":{\"ORG\":[\"Macys\",\"\"]}}"

    it "BEGIN:VCARD\nORG:Macys;\nBDAY:2014-06-09\n  (Wednesday)\nNOTE:Has Immunization Record\nEND:VCARD" $
      jsonTest (vcf []) "BEGIN:VCARD\nORG:Macys;\nBDAY:2014-06-09-\n Wednesday\nNOTE:Has Immunization Record\nEND:VCARD" `shouldBe` "[{\"fields\":{\"ORG\":[\"Macys\",\"\"],\"NOTE\":\"Has Immunization Record\",\"BDAY\":\"2014-06-09-Wednesday\"}}]"

    it "BEGIN:VCARD\nORG:Macys;\nEND:VCARD" $
      jsonTest (vcf []) "BEGIN:VCARD\nORG:Macys\nEND:VCARD" `shouldBe` "[{\"fields\":{\"ORG\":\"Macys\"}}]"

    it "BEGIN:VCARD\nORG:Macys\nEND:VCARD\nBEGIN:VCARD\nORG:TargetEND:VCARD" $
      jsonTest (vcf []) "BEGIN:VCARD\nORG:Macys\nEND:VCARD\nBEGIN:VCARD\nORG:Target\nEND:VCARD" `shouldBe` "[{\"fields\":{\"ORG\":\"Macys\"}},{\"fields\":{\"ORG\":\"Target\"}}]"

    it "URL;type=WORK;type=pref:mychart.tpcllp.com/MyChart/" $
      jsonTest urlField "URL;type=WORK;type=pref:mychart.tpcllp.com/MyChart/" `shouldBe` "{\"mychart.tpcllp.com/MyChart/\":[{\"type\":\"WORK\"},{\"type\":\"pref\"}]}"

    it "item1.URL;type=pref:www.firststudentinc.com" $
      jsonTest urlField "item1.URL;type=pref:www.firststudentinc.com" `shouldBe` "{\"www.firststudentinc.com\":{\"type\":\"pref\"}}"

    it "item1.URL;type=pref:biocircuits.ucsd.edu/nmpinter/Greening%20et%20al%202015%20CZ.pdf" $
      jsonTest urlField "item1.URL;type=pref:biocircuits.ucsd.edu/nmpinter/Greening%20et%20al%202015%20CZ.pdf" `shouldBe` "{\"biocircuits.ucsd.edu/nmpinter/Greening%20et%20al%202015%20CZ.pdf\":{\"type\":\"pref\"}}"
