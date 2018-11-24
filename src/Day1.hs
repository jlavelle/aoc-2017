module Day1 where

import Control.Lens (ifoldMap)
import Data.Monoid (Sum(..))
import Data.List (unfoldr)

digits :: Integer -> [Integer]
digits = reverse . unfoldr go
 where
  go 0 = Nothing
  go i = Just (i `mod` 10, i `div` 10)

solve1 :: Integer -> Integer
solve1 i = getSum $ ifoldMap go ds
 where
  ds = digits i
  go k a | cd !! succ k == a = Sum a
         | otherwise         = Sum 0
   where
    cd = cycle ds

solve2 :: Integer -> Integer
solve2 i = getSum $ ifoldMap go ds
 where
  ds = digits i
  go k a | cd !! x == a = Sum a
         | otherwise    = Sum 0
   where
    x  = k + div l 2
    cd = cycle ds
    l  = length ds

tests :: [Bool]
tests =
  [ solve1 1122     == 3
  , solve1 1111     == 4
  , solve1 1234     == 0
  , solve1 91212129 == 9
  , solve2 1212     == 6
  , solve2 1221     == 0
  , solve2 123425   == 4
  , solve2 123123   == 12
  , solve2 12131415 == 4
  ]
