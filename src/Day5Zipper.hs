{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Day5Zipper where

import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Data.List (unfoldr)
import Day5 (rule1, rule2)

-- This solution is extremely slow compared to the Vector version

data Tape a = Tape ![a] a ![a] deriving Show

readFocus :: Tape a -> a
readFocus (Tape _ a _) = a

left :: Tape a -> Maybe (Tape a)
left (Tape [] _ _)      = Nothing
left (Tape (l:ls) f xs) = Just $ Tape ls l (f:xs)

right :: Tape a -> Maybe (Tape a)
right (Tape _ _ [])      = Nothing
right (Tape ls f (x:xs)) = Just $ Tape (f:ls) x xs

modifyFocus :: (a -> a) -> Tape a -> Tape a
modifyFocus f (Tape ls a rs) = Tape ls (f a) rs

leftN :: Int -> Tape a -> Maybe (Tape a)
leftN 0 t = Just t
leftN n t = left t >>= leftN (n - 1)

rightN :: Int -> Tape a -> Maybe (Tape a)
rightN 0 t = Just t
rightN n t = right t >>= rightN (n - 1)

unfoldTape :: (Int -> Int) -> Tape Int -> [Tape Int]
unfoldTape f = unfoldr (\t -> (t,) <$> stepTape f t)

stepTape :: (Int -> Int) -> Tape Int -> Maybe (Tape Int)
stepTape f t = step . modifyFocus f $ t
 where
  pos = readFocus t
  step | pos >= 0  = rightN pos
       | otherwise = leftN (abs pos)

solve1 :: Tape Int -> Int
solve1 = (+1) . length . unfoldTape rule1

solve2 :: Tape Int -> Int
solve2 = (+1) . length . unfoldTape rule2

solveIO :: (Tape Int -> Int) -> IO Int
solveIO f = f . either error id . parse <$> T.readFile "./input/Day5.txt"

parse :: Text -> Either String (Tape Int)
parse = fmap (\(x:xs) -> Tape [] x xs) . traverse (fmap fst . T.signed T.decimal) . T.lines
