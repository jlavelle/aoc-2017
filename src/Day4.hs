{-# LANGUAGE OverloadedStrings #-}

module Day4 where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Set as S
import Data.List (sort)
import Data.Functor.Foldable (hylo)
import Data.Either (either)
import Control.Lens (partsOf, each, (%~), (&))

solve1 :: [Text] -> Int
solve1 = length . filter valid
 where
  valid l = S.toAscList ss == ws
   where
    ws = sort $ T.words l
    ss = S.fromAscList ws

solve2 :: [Text] -> Int
solve2 = length . filter (noAnagrams . T.words)

noAnagrams :: [Text] -> Bool
noAnagrams ts = hylo (either id id) coalg (ts, mempty)
 where
  coalg ([], _)   = Left True
  coalg (x:xs, s) | S.member x' s = Left False
                  | otherwise     = Right (xs, S.insert x' s)
   where
    x' = x & partsOf each %~ sort

solveIO :: ([Text] -> Int) -> IO Int
solveIO f = f . T.lines <$> T.readFile "./input/Day4.txt"

tests :: [Bool]
tests =
  [ solve1 ["aa aa", "aa bb"] == 1
  , solve2 ["ab ba"] == 0
  , solve2 ["ab bc", "abc cba"] == 1
  , solve2 ["aa aa"] == 0
  ]
