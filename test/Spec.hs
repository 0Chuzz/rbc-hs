{-#LANGUAGE FlexibleInstances #-}
import Test.Hspec
import Data.Circuits.Aig as R
import Test.QuickCheck
import Test.QuickCheck.Function
import Control.Monad

randomFlip :: Gen AigNode -> Gen AigEdge
randomFlip = liftM2 AigEdge  (arbitrary :: Gen Bool)


arbVar = elements ["a", "b", "c", "x", "y"]

sizedArbRBC 0 = oneof (map randomFlip [return AigTrue, fmap AigVar arbVar ])

sizedArbRBC n = randomFlip $ liftM2 AigAnd (sizedArbRBC (n-1)) (sizedArbRBC (n-1))

instance Arbitrary AigEdge where
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
  describe "Quickcheck" $ do
    it "is quick checked" $ property $ \(x,y) -> let assgm = (apply (y :: Fun String Bool)) in R.eval assgm x /= R.eval assgm (R.not x)
