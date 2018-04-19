module CardSpec where

import Test.Hspec
import Test

spec :: Spec
spec = do
  describe "jsonTest" $ do
    it "X-ABUID:4709EC50-7594-4F67-85E1-6870DA65FCBA:ABPerson\n -> junk" $
      "gronk" `shouldBe` "junk"
