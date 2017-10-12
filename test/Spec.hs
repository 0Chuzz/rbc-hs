import Test.Hspec
import Data.Rbc as R


exAssgm "a" = True
exAssgm "b" = False
exAssgm other = error ("not defined: " ++ other)

main :: IO ()
main = hspec $ do
  describe "Base lib" $ do
    it "should do something" $ do
      R.eval exAssgm (R.or (R.var "a") (R.var "b")) `shouldBe` True
