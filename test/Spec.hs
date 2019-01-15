import Data
import Lib
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "FormatGrid" $ do
      it "Should format grid using unilines" $ do
        (formatGrid ["abc", "def"]) `shouldBe` "abc\ndef\n"
