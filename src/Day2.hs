module Day2 where

import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.IO  as T (readFile)
import Data.Text (Text)
import Control.Lens (alaf)
import qualified Control.Foldl as F
import Control.Applicative (liftA2)
import Data.Monoid (Sum(..))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust)
import Control.Monad ((<=<))

-- using NonEmpty so that fromJust is not partial
solve :: [NonEmpty Int] -> Int
solve = alaf Sum foldMap (fromJust . go)
 where
  go = F.fold $ (liftA2 . liftA2) (-) F.maximum F.minimum

parse :: Text -> Either String [NonEmpty Int]
parse = traverse f . T.lines
 where
  f = fromList' <=< traverse (fmap fst . T.decimal) . T.words

fromList' :: [a] -> Either String (NonEmpty a)
fromList' [] = Left "fromList': Empty List"
fromList' as = Right $ NE.fromList as

getInput :: IO [NonEmpty Int]
getInput = (either error id . parse) <$> T.readFile "./input/Day2.txt"

tests :: [Bool]
tests =
  [ solve [ NE.fromList [5, 1, 9, 5]
          , NE.fromList [7, 5, 3]
          , NE.fromList [2, 4, 6, 8]
          ] == 18
  ]
