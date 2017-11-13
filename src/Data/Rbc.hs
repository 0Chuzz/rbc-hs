{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Rbc
     where
import Prelude hiding (and, not)
import qualified Data.Bool as B 
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Bits (shiftL, shiftR)
import Data.Hashable

data RbcNode =
  RbcTrue
  | RbcVar String
  | RbcAnd RbcEdge RbcEdge
  deriving (Show)

instance Eq RbcNode where
  (==) RbcTrue RbcTrue = True
  (==) RbcTrue _ = False
  (==) a RbcTrue = RbcTrue == a
  (==) _ _ = False

instance Hashable RbcNode where
  hashWithSalt salt RbcTrue = salt + 1
  hashWithSalt salt (RbcVar s) = 1 + hashWithSalt salt s
  hashWithSalt salt (RbcAnd a b) = 1 + hashWithSalt salt a + hashWithSalt salt b

data RbcEdge = RbcEdge Bool RbcNode
  deriving (Show, Eq)

instance Hashable RbcEdge where
  hashWithSalt salt (RbcEdge f n) = hashWithSalt salt f + hashWithSalt salt n


class BoolFunc a where
  truth :: a
  negation :: a -> a
  conjunction :: a -> a -> a

instance BoolFunc Bool where
  truth = True
  negation = B.not
  conjunction = (&&)

instance BoolFunc RbcEdge where
  truth = RbcEdge False RbcTrue
  negation = not
  conjunction = and

instance BoolFunc [Bool] where
  truth = repeat True
  negation a = map B.not a
  conjunction a b =  [x && y | x <-a | y<-b ]

var a = RbcEdge False $ RbcVar a

and (RbcEdge True RbcTrue) b = b
and a@(RbcEdge False RbcTrue) b = a
and a b@(RbcEdge _ RbcTrue) = and b a
and a@(RbcEdge f1 n1) b@(RbcEdge f2 n2) = if n1 == n2 then
                                            if f1 == f2 then a else RbcEdge True RbcTrue
                                          else RbcEdge False $ RbcAnd a b
not (RbcEdge flip node) = RbcEdge (B.not flip) node
or a b = not $ and (not a) (not b)



eval :: BoolFunc b => (String-> b) -> RbcEdge -> b
eval assgm (RbcEdge flip n) = if flip then negation val else val
  where
    val = evalNode assgm n

evalNode assgm RbcTrue = truth
evalNode assgm (RbcVar s) = assgm s
evalNode assgm (RbcAnd a b) = conjunction (eval assgm a) (eval assgm b)

truthTable :: [String] -> RbcEdge ->  [Bool]
truthTable inputs formula = eval ttAssgm formula
  where
    tabsize = 1 `shiftL` (length inputs)
    ttAssgm a = allAssgms (length inputs)!! fromJust (elemIndex a inputs)
    allAssgms ::  Int -> [[Bool]]
    allAssgms size = [assRow tabsize i | i<-[1..size]]
    assRow size 0= take size (repeat True)
    assRow size n = assRow (size `shiftR` 1) (n-1) ++ map B.not (assRow (size `shiftR` 1) (n-1))

someFunc :: IO ()
someFunc = putStrLn "someFunc"
