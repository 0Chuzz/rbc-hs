{-#LANGUAGE FlexibleInstances, FlexibleContexts #-}
import Test.Hspec
import Data.Circuits.Aig as R
import Data.Circuits.Aiger as Aiger
import qualified Text.Parsec.ByteString.Lazy as PRS
import Test.QuickCheck
import Test.QuickCheck.Function
import Control.Monad
import qualified Data.ByteString.Lazy as BS
import qualified Text.Parsec as Parsec
import Data.Either(isRight)

  
randomFlip :: Gen AigNode -> Gen AigEdge
randomFlip = liftM2 AigEdge  (arbitrary :: Gen Bool)


arbVar = elements ["a", "b", "c", "x", "y"]

sizedArbRBC 0 = oneof (map randomFlip [return AigTrue, fmap AigVar arbVar ])

sizedArbRBC n = randomFlip $ liftM2 AigAnd (sizedArbRBC (n `div` 2)) (sizedArbRBC (n `div` 2))

instance Arbitrary AigEdge where
  arbitrary = sized sizedArbRBC


exAssgm "a" = True
exAssgm "b" = False
exAssgm "c" = False
exAssgm "x" = False
exAssgm "y" = True
exAssgm other = error ("not defined: " ++ other)

beintTest = [
    (0, BS.pack [0x00]),
    (1, BS.pack [0x01]),
    (0x7F, BS.pack [0x7f]),
    (0x80, BS.pack [0x80, 0x01]),
    (0xFFFF, BS.pack [0xff, 0xff, 0x03]),
    (0xFFFFFFFF, BS.pack [0xff,0xff,0xff,0xff,0x0f]),
    (0x11223344, BS.pack [0xc4,0xe6,0x88,0x89,0x01])]

main :: IO ()
main = hspec $ do
  describe "Base lib" $ 
    it "should do something" $ 
      R.eval exAssgm (R.or (R.var "a") (R.var "b")) `shouldBe` True
  describe "Quickcheck" $ do
    it "is quick checked" $ 
      property $ \(x,y) -> let assgm = (apply (y :: Fun String Bool))
                          in R.eval assgm x /= R.eval assgm (R.not x)
  describe "Aiger parser" $ do
    it "parses integer correctly" $ do
      let testMe (num, byts) = Parsec.parse beint "" byts `shouldBe` Right num
      forM beintTest testMe
      return ()
    it "parses an aig file" $ do
      contents <- PRS.parseFromFile Aiger.aigFile "test/data/c17.aig"
      contents `shouldSatisfy` isRight
