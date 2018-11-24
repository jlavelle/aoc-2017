module Util where

data Turn = L | R | N | U deriving Show

rotateEnum :: (Eq a, Enum a, Bounded a) => Turn -> a -> a
rotateEnum L c | c == minBound = maxBound
               | otherwise     = pred c
rotateEnum R c | c == maxBound = minBound
               | otherwise     = succ c
rotateEnum N c = c
rotateEnum U c = rotateEnum L $ rotateEnum L c
