module ScrabbleScore where

import Prelude(flip, map, (+), (>>>))

import Data.Array (catMaybes, foldl)
import Data.HashMap (HashMap, fromFoldable, lookup)
import Data.String (toUpper)
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))

lettersWithScores :: Array (Tuple Char Int)
lettersWithScores =
  [
    (Tuple 'A' 1),
    (Tuple 'E' 1),
    (Tuple 'I' 1),
    (Tuple 'O' 1),
    (Tuple 'U' 1),
    (Tuple 'L' 1),
    (Tuple 'N' 1),
    (Tuple 'R' 1),
    (Tuple 'S' 1),
    (Tuple 'T' 1),
    (Tuple 'D' 2),
    (Tuple 'G' 2),
    (Tuple 'B' 3),
    (Tuple 'C' 3),
    (Tuple 'M' 3),
    (Tuple 'P' 3),
    (Tuple 'F' 4),
    (Tuple 'H' 4),
    (Tuple 'V' 4),
    (Tuple 'W' 4),
    (Tuple 'Y' 4),
    (Tuple 'K' 5),
    (Tuple 'J' 8),
    (Tuple 'X' 8),
    (Tuple 'Q' 10),
    (Tuple 'Z' 10)
  ]

lsHashMap :: HashMap Char Int
lsHashMap = fromFoldable lettersWithScores

scoreWord :: String -> Int
scoreWord "" = 0
scoreWord s = (toUpper
               >>> toCharArray
               >>> (map (flip lookup lsHashMap))
               >>> catMaybes
               >>> (foldl (+) 0)
              ) s
