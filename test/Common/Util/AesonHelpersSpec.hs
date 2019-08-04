module Common.Util.AesonHelpersSpec (spec) where

import Test.Hspec
import qualified Common.Util.AesonHelpers as Helpers

spec :: Spec
spec = do
  describe "dropPrefix_" $ do
    it "should drop everything until the first _" $
      Helpers.dropPrefix_ "asd_foobar" `shouldBe` "foobar"
    it "should do nothing if there is no _" $
      Helpers.dropPrefix_ "foobar" `shouldBe` "foobar"
    it "should not drop beyond the first _" $
      Helpers.dropPrefix_ "asd_foo_bar" `shouldBe` "foo_bar"
