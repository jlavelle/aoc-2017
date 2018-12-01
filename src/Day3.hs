{-# LANGUAGE TupleSections #-}

module Day3 where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set (Set)
import Data.Map (Map)
import Data.Foldable (fold)
import Data.List (nub, sortBy, unfoldr)
import Data.Maybe (fromJust, isJust, catMaybes)
import Util (rotateEnum, Turn(..))

-- Automaton solution
solve1 :: Int -> Int
solve1 n = absMoves . pos . walker $ ss !! (n - 1)
 where
  ss = unfoldSystem rule1

solve2 :: Int -> Int
solve2 x = lastWrite . walker . head . dropWhile ((>) x . lastWrite . walker) $ unfoldSystem rule2

solve2At :: Int -> Int
solve2At n = lastWrite . walker $ unfoldSystem rule2 !! n

unfoldSystem :: Rule -> [System]
unfoldSystem r = unfoldr go initSystem
 where
  go s = Just (s, stepSystem r s)

data Direction
  = North
  | East
  | South
  | West
  deriving (Eq, Enum, Bounded, Show)

data Walker = Walker
  { lastWrite :: Int
  , pos       :: (Int, Int)
  , direction :: Direction
  }
  deriving Show

data System = System
  { walker :: Walker
  , cells  :: Map (Int, Int) Int
  } deriving Show

type Rule = Walker -> Map (Int, Int) Int -> (Walker, Int)

initSystem :: System
initSystem = System (Walker 1 (0, 0) East) M.empty

stepSystem :: Rule -> System -> System
stepSystem r (System w cs) =
  let (w', c) = r w cs
  in System w' (M.insert (pos w) c cs)

forward :: Walker -> Walker
forward (Walker w p d) = Walker w (move d p) d

move :: Direction -> (Int, Int) -> (Int, Int)
move d (x, y) = case d of
  North -> (x, y + 1)
  West  -> (x - 1, y)
  South -> (x, y - 1)
  East  -> (x + 1, y)

-- How to write the next cell based on the current state
type Write = Walker -> Map (Int, Int) Int -> Int

makeRule :: Write -> Rule
makeRule f wa@(Walker _ p d) cs | rear && not left = (forward $ Walker nw p $ rotateEnum L d, nw)
                                | otherwise        = (forward $ Walker nw p d, nw)
 where
  nw = f wa cs
  rear = isJust $ relativeNeighbor p South d cs
  left = isJust $ relativeNeighbor p West d cs

rule1 :: Rule
rule1 = makeRule write
 where
  write (Walker lw _ _) _ = lw + 1

rule2 :: Rule
rule2 = makeRule write
 where
  write (Walker _ p _) cs = if s == 0 then 1 else s
   where
    s = sum . catMaybes $ fmap (flip M.lookup cs) (neighbors p)

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(a, b) | a <- f x, b <- f y, (a, b) /= (x, y)]
 where
  f z = [z, z + 1, z - 1]

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
    l = [(2 * n - 1) ^ 2 + 1..(2 * n + 1) ^ 2]

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

tests :: [Bool]
tests =
  [ solve1' 289326 == 419
  , solve1  289326 == 419
  , solve2At 1     == 1
  , solve2At 2     == 1
  , solve2At 3     == 2
  , solve2At 13    == 59
  ]
