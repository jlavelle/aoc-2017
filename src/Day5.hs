{-# LANGUAGE TupleSections #-}

module Day5 where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import Data.Vector (Vector, (!?))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.List (unfoldr)
import Control.Applicative (liftA2)

data Program = Program
  { jumps    :: Vector Int
  , position :: Int
  } deriving Show

initProgram :: Vector Int -> Program
initProgram v = Program v 0

readPos :: Program -> Maybe Int
readPos (Program js p) = js !? p

incJump :: Program -> Program
incJump (Program js p) = Program (V.modify (\v -> MV.modify v (+1) p) js) p

jump :: Program -> Maybe Program
jump p = (\z -> p' { position = position p' + z }) <$> readPos p
 where
  p' = incJump p

unfoldProgram :: Program -> [Program]
unfoldProgram = unfoldr (\p -> (p,) <$> jump p)

solve1 :: Program -> Int
solve1 = length . unfoldProgram

tests :: [Bool]
tests =
  [ solve1 (Program (V.fromList [0, 3, 0, 1, -3]) 0) == 5
  ]

solveIO :: (Program -> Int) -> IO Int
solveIO f = solve1 . either error id . parse <$> T.readFile "./input/Day5.txt"

parse :: Text -> Either String Program
parse = fmap initProgram . traverse (fmap fst . T.signed T.decimal) . V.fromList . T.lines

