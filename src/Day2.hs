{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day2 where

import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.IO  as T (readFile)
import Data.Text (Text)
import Control.Lens (alaf, (&), partsOf, each, (%~))
import qualified Control.Foldl as F
import Control.Applicative (liftA2)
import Data.Monoid (Sum(..), First(..))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust)
import Data.List (sortBy)
import Data.Functor.Foldable (para, ListF(..))

-- keeping this here as an example of using alaf
solve1' :: (Foldable f, Foldable g, Num a, Ord a) => f (g a) -> a
solve1' = alaf Sum foldMap (maybe 0 id . go)
 where
  go = F.fold $ (liftA2 . liftA2) (-) F.maximum F.minimum

-- using NonEmpty so that fromJust is not partial
solve1 :: [NonEmpty Int] -> Int
solve1 = sum . fmap (fromJust . go)
 where
  go = F.fold $ (liftA2 . liftA2) (-) F.maximum F.minimum

-- The problem doesn't require handling edge cases, but we're doing it anyway
solve2 :: [[Int]] -> Maybe Int
solve2 = fmap getSum . foldMap go
 where
  go as = paired as
        & traverse . partsOf each %~ sortBy (flip compare)
        & filter ((==) 0 . uncurry rem)
        & fmap (uncurry div)
        & alaf First foldMap (Just . Sum)

paired :: [a] -> [(a, a)]
paired = para alg
 where
  alg Nil = []
  alg (Cons x (xs, r)) = fmap (x,) xs <> r

tests :: [Bool]
tests =
  [ solve1 [ NE.fromList [5, 1, 9, 5]
           , NE.fromList [7, 5, 3]
           , NE.fromList [2, 4, 6, 8]
           ] == 18
  , solve2 [ [5, 9, 2, 8]
           , [9, 4, 7, 3]
           , [3, 8, 6, 5]
           ] == Just 9
  ]

parse :: Text -> Either String [[Int]]
parse = traverse f . T.lines
 where
  f = traverse (fmap fst . T.decimal) . T.words

getInput :: IO [[Int]]
getInput = either error id . parse <$> T.readFile "./input/Day2.txt"

