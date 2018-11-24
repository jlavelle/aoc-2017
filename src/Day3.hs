module Day3 where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set (Set)
import Data.Map (Map)
import Data.Foldable (fold)
import Data.List (nub, sortBy)
import Data.Maybe (fromJust)

solve :: Int -> Int
solve x = f $ fromJust $ M.lookup x $ layer l
 where
  f (a, b) = abs a + abs b
  l = div (ceiling . sqrt . fromIntegral $ x) 2

layer :: Int -> Map Int (Int, Int)
layer n = M.fromList $ zip (members n) (points n)

members :: Int -> [Int]
members 0 = [1]
members n = rotate (length l - 1) l
 where
  l = [(2 * (n - 1) + 1) ^ 2 + 1..((2 * n + 1) ^ 2)]

points :: Int -> [(Int, Int)]
points n = nub $ fold
  [ fmap (n,)  ps
  , fmap (,n)  rps
  , fmap (-n,) rps
  , fmap (,-n) ps
  ]
 where
  ps  = [-n..n]
  rps = reverse ps

rotate :: Int -> [a] -> [a]
rotate n as = take (length as) (drop n $ cycle as)
