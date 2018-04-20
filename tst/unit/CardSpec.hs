module CardSpec where

import Test.Hspec
import Test
import Parse
import Data.Aeson as Aeson (ToJSON(..), encode)
import Data.Text as T (unpack, pack)
import Data.ByteString.Lazy.Char8 as DBLC8 (pack)

spec :: Spec
spec = do
  describe "jsonTest" $ do
    it "X-ABUID:4709EC50-7594-4F67-85E1-6870DA65FCBA:ABPerson\n -> junk" $
      jsonTest attribute "a=b" `shouldBe` "{\"name\":\"a\",\"value\":\"b\"}"

    it "ab:\n" $
      jsonTest field "ab:\n" `shouldBe` "{\"ab\":[{\"name\":\"\"}]}"

    it "ab:c\n" $
      jsonTest field "ab:c\n" `shouldBe` "{\"ab\":[{\"name\":\"c\"}]}"

    it "N:;;;;\n" $
      jsonTest field "N:;;;;\n" `shouldBe` "{\"N\":[{\"name\":\"\"},{\"name\":\"\"},{\"name\":\"\"},{\"name\":\"\"},{\"name\":\"\"}]}"

    it "PRODID:-//Apple Inc.//Mac OS X 10.13.4//EN\n" $
      jsonTest field "PRODID:-//Apple Inc.//Mac OS X 10.13.4//EN\n" `shouldBe` "{\"PRODID\":[{\"name\":\"-//Apple Inc.//Mac OS X 10.13.4//EN\"}]}"

    it ":kdkdk\n" $ -- Should fail (left "(unknown)")
      jsonTest field ":kdkdk\n" `shouldBe` "\"parse Failure\" (line 1, column 1):\nunexpected \":\""

    it "ORG:Macys;\n" $
      jsonTest field "ORG:Macys;\n" `shouldBe` "{\"ORG\":[{\"name\":\"Macys\"},{\"name\":\"\"}]}"

    it "\n" $ -- Should fail, empty line not allowed
      jsonTest field "\n" `shouldBe` "\"parse Failure\" (line 1, column 1):\nunexpected \"\\n\""

    it "TEL;type=CELL;type=VOICE;type=pref:15036451141\n" $
      jsonTest field "TEL;type=CELL;type=VOICE;type=pref:15036451141\n" `shouldBe` "{\"TEL\":[{\"value\":\"CELL\",\"name\":\"type\"},{\"value\":\"VOICE\",\"name\":\"type\"},{\"value\":\"pref\",\"name\":\"type\"},{\"name\":\"15036451141\"}]}"
      
    it "ORG:Macys;\n -kdkdkdkd\n" $ -- Should fail because of '-' in the continuation
      jsonTest field "ORG:Macys;\n -kdkdkdkd\n" `shouldBe` "\"parse Failure\" (line 2, column 2):\nunexpected \"-\"\nexpecting letter or digit or \"\\n\""

    it "ORG:Macys--\n mcmcmcmc\n" $
      jsonTest field "ORG:Macys--\n mcmcmcmc\n" `shouldBe` "{\"ORG\":[{\"value\":\"Macys--mcmcmcmc\",\"name\":\"Continuation\"}]}"

    it "\n mcmcmcmc\n" $
      jsonTest field "\n mcmcmcmc\n" `shouldBe` "\"parse Failure\" (line 1, column 1):\nunexpected \"\\n\""

    it "BEGIN:VCARD\nORG:Macys;\nEND:VCARD\n" $
      jsonTest card "BEGIN:VCARD\nORG:Macys;\nEND:VCARD\n" `shouldBe` "{\"fields\":[{\"ORG\":[{\"name\":\"Macys\"},{\"name\":\"\"}]}]}"

    it "BEGIN:VCARD\nORG:Macys;\nBDAY:2014-06-09\n continue\nNOTE:Has Immunization Record\nEND:VCARD" $
      jsonTest vcf "BEGIN:VCARD\nORG:Macys;\nBDAY:2014-06-09\n continue\nNOTE:Has Immunization Record\nEND:VCARD" `shouldBe` "{\"cards\":[{\"fields\":[{\"ORG\":[{\"name\":\"Macys\"},{\"name\":\"\"}]},{\"BDAY\":[{\"value\":\"2014-06-09continue\",\"name\":\"Continuation\"}]},{\"NOTE\":[{\"name\":\"Has Immunization Record\"}]}]}]}"

    it "BEGIN:VCARD\nORG:Macys;\nEND:VCARD" $
      jsonTest vcf "BEGIN:VCARD\nORG:Macys;\nEND:VCARD" `shouldBe` "{\"cards\":[{\"fields\":[{\"ORG\":[{\"name\":\"Macys\"},{\"name\":\"\"}]}]}]}"

