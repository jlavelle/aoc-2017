{-# LANGUAGE ViewPatterns #-}

module Day1 where

import Control.Lens (ifoldMap)
import Data.Monoid (Sum(..))
import Data.List (unfoldr)
import Data.Functor.Foldable (ana, ListF(..))

digits' :: Integral a => a -> [a]
digits' = ana go
 where
  go 0 = Nil
  go i = Cons (i `mod` 10) (i `div` 10)

digits :: Integral a => a -> [a]
digits = unfoldr go
 where
  go 0 = Nothing
  go i = Just (i `mod` 10, i `div` 10)

-- TODO: can we use alaf here or not?
gsolve :: Integral a => (Int -> [a] -> a) -> a -> a
gsolve f (digits -> ds) = getSum $ ifoldMap go ds
 where
  go k a | f k ds == a = Sum a
         | otherwise   = Sum 0

solve1 :: Integral a => a -> a
solve1 = gsolve (\i as -> cycle as !! succ i)

solve2 :: Integral a => a -> a
solve2 = gsolve (\i as -> cycle as !! (i + div (length as) 2))

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
