{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Circuits.Aig
     where
import Prelude hiding (and, not)
import qualified Data.Bool as B 
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Bits (shiftL, shiftR)
import Data.Hashable

data AigNode=
  AigTrue
  | AigVar String
  | AigAnd AigEdge AigEdge
  deriving (Show)

instance Eq AigNode where
  (==) AigTrue AigTrue = True
  (==) AigTrue _ = False
  (==) a AigTrue = AigTrue == a
  (==) _ _ = False

instance Hashable AigNode where
  hashWithSalt salt AigTrue = salt + 1
  hashWithSalt salt (AigVar s) = 1 + hashWithSalt salt s
  hashWithSalt salt (AigAnd a b) = 1 + hashWithSalt salt a + hashWithSalt salt b

data AigEdge = AigEdge Bool AigNode
  deriving (Show, Eq)

instance Hashable AigEdge where
  hashWithSalt salt (AigEdge f n) = hashWithSalt salt f + hashWithSalt salt n


class BoolFunc a where
  truth :: a
  negation :: a -> a
  conjunction :: a -> a -> a

instance BoolFunc Bool where
  truth = True
  negation = B.not
  conjunction = (&&)

instance BoolFunc AigEdge where
  truth = AigEdge False AigTrue
  negation = not
  conjunction = and

instance BoolFunc [Bool] where
  truth = repeat True
  negation a = map B.not a
  conjunction a b =  [x && y | x <-a | y<-b ]

var a = AigEdge False $ AigVar a

and :: AigEdge -> AigEdge -> AigEdge
and (AigEdge True AigTrue) b = b
and a@(AigEdge False AigTrue) b = a
and a b@(AigEdge _ AigTrue) = and b a
and a@(AigEdge f1 n1) b@(AigEdge f2 n2) = if n1 == n2 then
                                            if f1 == f2 then a else AigEdge True AigTrue
                                          else AigEdge False $ AigAnd a b

not :: AigEdge -> AigEdge
not (AigEdge flip node) = AigEdge (B.not flip) node
or a b = not $ and (not a) (not b)



eval :: BoolFunc b => (String-> b) -> AigEdge -> b
eval assgm (AigEdge flip n) = if flip then negation val else val
  where
    val = evalNode assgm n

evalNode assgm AigTrue = truth
evalNode assgm (AigVar s) = assgm s
evalNode assgm (AigAnd a b) = conjunction (eval assgm a) (eval assgm b)

truthTable :: [String] -> AigEdge ->  [Bool]
truthTable inputs formula = eval ttAssgm formula
  where
    tabsize = 1 `shiftL` length inputs
    ttAssgm a = allAssgms (length inputs)!! fromJust (elemIndex a inputs)
    allAssgms ::  Int -> [[Bool]]
    allAssgms size = [assRow tabsize i | i<-[1..size]]
    assRow size 0= replicate size True
    assRow size n = assRow (size `shiftR` 1) (n-1) ++ map B.not (assRow (size `shiftR` 1) (n-1))

someFunc :: IO ()
someFunc = putStrLn "someFunc"
