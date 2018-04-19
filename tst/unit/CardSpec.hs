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
