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

data RbcNode =
  RbcTrue
  | RbcVar String
  | RbcAnd RbcEdge RbcEdge
  deriving (Show)

data RbcEdge = RbcEdge Bool RbcNode
  deriving (Show)


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
and a b = RbcEdge False $ RbcAnd a b
not (RbcEdge flip node) = RbcEdge (B.not flip) node
or a b = not $ and (not a) (not b)


eval :: BoolFunc b => (String-> b) -> RbcEdge -> b
eval assgm (RbcEdge flip RbcTrue) = if flip then negation truth else truth 
eval assgm (RbcEdge flip (RbcVar s)) = assgm s
eval assgm (RbcEdge flip (RbcAnd a b)) = if flip then negation result else result
  where
    result = conjunction (eval assgm a) (eval assgm b)

tabletruth :: [String] -> RbcEdge ->  [Bool]
tabletruth inputs formula = eval ttAssgm formula
  where
    tabsize = 1 `shiftL` (length inputs)
    ttAssgm a = allAssgms (length inputs)!! fromJust (elemIndex a inputs)
    allAssgms ::  Int -> [[Bool]]
    allAssgms size = [assRow tabsize i | i<-[1..size]]
    assRow size 0= take size (repeat True)
    assRow size n = assRow (size `shiftR` 1) (n-1) ++ map B.not (assRow (size `shiftR` 1) (n-1))

someFunc :: IO ()
someFunc = putStrLn "someFunc"
