{-# LANGUAGE TupleSections #-}

module Day3 where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set (Set)
import Data.Map (Map)
import Data.Foldable (fold)
import Data.List (nub, sortBy, unfoldr)
import Data.Maybe (fromJust, isJust)
import Util (rotateEnum, Turn(..))

-- Automaton solution
solve1 :: Int -> Int
solve1 n = absMoves . pos . walker $ ss !! (n - 1)
 where
  ss = unfoldr go initSystem
   where
    go s = Just (s, stepSystem rule1 s)

data Direction
  = North
  | East
  | South
  | West
  deriving (Eq, Enum, Bounded, Show)

data Walker = Walker Int (Int, Int) Direction deriving Show

pos :: Walker -> (Int, Int)
pos (Walker _ p _) = p

steps :: Walker -> Int
steps (Walker s _ _) = s

data System = System
  { walker :: Walker
  , cells  :: Map (Int, Int) Int
  } deriving Show

initSystem :: System
initSystem = System (Walker 1 (0, 0) East) M.empty

stepSystem :: (Walker -> Map (Int, Int) Int -> (Walker, Int)) -> System -> System
stepSystem r (System w cs) =
  let (w', c) = r w cs
  in System w' (M.insert (pos w) c cs)

forward :: Walker -> Walker
forward (Walker s p d) = Walker (s + 1) (move d p) d

move :: Direction -> (Int, Int) -> (Int, Int)
move d (x, y) = case d of
  North -> (x, y + 1)
  West  -> (x - 1, y)
  South -> (x, y - 1)
  East  -> (x + 1, y)

rule1 :: Walker -> Map (Int, Int) Int -> (Walker, Int)
rule1 w@(Walker s p d) cs | rear && not left = (forward $ Walker s p $ rotateEnum L d, s)
                          | otherwise        = (forward w, s)
 where
  rear = isJust $ relativeNeighbor p South d cs
  left = isJust $ relativeNeighbor p West d cs

relativeNeighbor :: (Int, Int) -> Direction -> Direction -> Map (Int, Int) Int -> Maybe Int
relativeNeighbor p to from cs = M.lookup (move (relativeDirection to from) p) cs

relativeDirection :: Direction -> Direction -> Direction
relativeDirection d x = rotateEnum t x
 where
  t = case d of
    North -> N
    South -> U
    East  -> R
    West  -> L

-- Alternate, more direct solution to part 1
solve1' :: Int -> Int
solve1' x = absMoves $ fromJust $ M.lookup x $ layer l
 where
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

absMoves :: (Int, Int) -> Int
absMoves (x, y) = abs x + abs y
