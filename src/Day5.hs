{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Day5 where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import Data.Vector (Vector, (!?))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.List (unfoldr)

data Program f = Program
  { jumps    :: f Int
  , position :: Int
  }

deriving instance (forall a. Show a => Show (f a)) => Show (Program f)

initProgram :: Vector Int -> Program Vector
initProgram v = Program v 0

readPos :: Program Vector -> Maybe Int
readPos (Program js p) = js !? p

updateJump :: (Int -> Int) -> Program Vector -> Program Vector
updateJump f (Program js p) = Program (V.modify (\v -> MV.modify v f p) js) p

jump :: (Int -> Int) -> Program Vector -> Maybe (Program Vector)
jump f p = (\z -> p' { position = position p' + z }) <$> readPos p
 where
  p' = updateJump f p

unfoldProgram :: (Int -> Int) -> Program Vector -> [Program Vector]
unfoldProgram f = unfoldr (\p -> (p,) <$> jump f p)

-- This is no faster than the unfoldr version
iterProgram :: (Int -> Int) -> Program Vector -> Int
iterProgram f p = go (Just p, -1)
 where
  go (Nothing, acc) = acc
  go (Just  x, acc) = acc `seq` go (jump f x, acc + 1)

rule1 :: Int -> Int
rule1 x = x + 1

rule2 :: Int -> Int
rule2 x | x >= 3    = x - 1
        | otherwise = x + 1

solve1 :: Program Vector -> Int
solve1 = length . unfoldProgram rule1

solve2 :: Program Vector -> Int
solve2 = length . unfoldProgram rule2

tests :: [Bool]
tests =
  [ solve1 (Program (V.fromList [0, 3, 0, 1, -3]) 0) == 5
  , solve2 (Program (V.fromList [0, 3, 0, 1, -3]) 0) == 10
  ]

solveIO :: (Program Vector -> Int) -> IO Int
solveIO f = f . either error id . parse <$> T.readFile "./input/Day5.txt"

parse :: Text -> Either String (Program Vector)
parse = fmap initProgram . traverse (fmap fst . T.signed T.decimal) . V.fromList . T.lines

