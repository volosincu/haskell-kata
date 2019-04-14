-- file Spec.hs
import Book
import Equals
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "Book data type" $ do
    it "Matematica" $ do
      Matematica `shouldBe` (Matematica::Book)
    it "Medicina" $ do
      Medicina `shouldBe` (Medicina::Book)
    it "Geografie" $ do
      Geografie `shouldBe` (Geografie::Book)
    it "Istorie" $ do
      Istorie `shouldBe` (Istorie::Book)

  describe "Test Equals Book instance - check Equals operator on book types" $ do
    it "Matematica <!!> Medicina are Equal" $ do
      Matematica <!!> Medicina `shouldBe` True
    it "Istorie <!!> Geografie are Equal" $ do
      Istorie <!!> Geografie `shouldBe` True
    it "Istorie <!!> Matematica aren't Equal" $ do
      Istorie <!!> Matematica `shouldBe` False
    it "Medicina <!!> Geografie aren't Equal" $ do
      Medicina <!!> Geografie `shouldBe` False

  describe "<!!> - partial apply" $ do
    it "(<!!> Medicina) shouldSatisfy Matematica" $ do
      (shouldSatisfy Matematica (<!!> Medicina))
    it "(<!!> Istorie) shouldSatisfy Geografie" $ do
      (shouldSatisfy Istorie (<!!> Geografie))
