{-# LANGUAGE TupleSections #-}

module Day5IntMap where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.List (unfoldr)

import Day5 (Program(..))
import qualified Day5

-- Using an IntMap is faster than Vector, but slightly slower than the Zipper

type Program' = Day5.Program IntMap

jump :: (Int -> Int) -> Program' -> Maybe Program'
jump f (Program js p) = fmap (Program js' . (+) p) $ M.lookup p js
 where
  js' = M.adjust f p js

unfoldProgram :: (Int -> Int) -> Program' -> [Program']
unfoldProgram f = unfoldr (\p -> (p,) <$> jump f p)

solve1 :: Program' -> Int
solve1 = length . unfoldProgram Day5.rule1

solve2 :: Program' -> Int
solve2 = length . unfoldProgram Day5.rule2

parse :: Text -> Either String Program'
parse = fmap (Day5.initProgram . M.fromList . zip [0..])
      . traverse (fmap fst . T.signed T.decimal)
      . T.lines

solveIO :: (Program' -> Int) -> IO Int
solveIO f = f . either error id . parse <$> T.readFile "./input/Day5.txt"
