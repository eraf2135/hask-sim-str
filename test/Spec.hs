import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Lib

main :: IO ()
main = hspec $ do
  describe "Closest String Test" $ do
    it "basic happy case" $ do
      closest ["foo bar", "bar foo", "foos bards", "foobar"] `shouldBe` (("foo bar", "foobar") :: ([Char], [Char]))
    it "empty collection" $ do
      evaluate (closest []) `shouldThrow` anyException
    it "single element collection" $ do
      evaluate (closest []) `shouldThrow` anyException
    it "2 element collection" $ do
      closest ["this is a test", "of two"] `shouldBe` (("this is a test", "of two") :: ([Char], [Char]))
