import Test.Hspec
import Data.Rbc as R
import Test.QuickCheck
import Control.Monad

randomFlip :: Gen RbcNode -> Gen RbcEdge
randomFlip n = do
  (liftM2 RbcEdge)  (arbitrary :: Gen Bool) n


sizedArbRBC 0 = oneof (map randomFlip [return RbcTrue, return (RbcVar "a"), return (RbcVar "b"), return (RbcVar "c"), return (RbcVar "x"), return (RbcVar "y")])

sizedArbRBC n = randomFlip $ (liftM2 RbcAnd) (sizedArbRBC (n-1)) (sizedArbRBC (n-1))

instance Arbitrary RbcEdge where
  arbitrary = sized sizedArbRBC


exAssgm "a" = True
exAssgm "b" = False
exAssgm "c" = False
exAssgm "x" = False
exAssgm "y" = True
exAssgm other = error ("not defined: " ++ other)

main :: IO ()
main = hspec $ do
  describe "Base lib" $ do
    it "should do something" $ do
      R.eval exAssgm (R.or (R.var "a") (R.var "b")) `shouldBe` True

    it "is quick checked" $ property $ \x -> R.eval exAssgm x /= R.eval exAssgm (R.not x)
