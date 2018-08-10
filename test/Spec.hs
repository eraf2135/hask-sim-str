import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Lib

main :: IO ()
main = hspec $ do
  describe "Closest String Test" $ do
    it "basic happy case" $ do
      closest ["foo bar", "bar foo", "foos bards", "foobar"] `shouldBe` (("foo bar", "foobar") :: ([Char], [Char]))
