module Day01 where

import           Data.List (find)
import           Data.Set  (Set)
import qualified Data.Set  as Set
import           Util


getInput :: String -> [Int]
getInput = map readSignedInt . lines

readSignedInt :: String -> Int
readSignedInt ('+':cs) = read cs
readSignedInt cs       = read cs

day01part1 :: String -> Int
day01part1 = sum . getInput

day01part2 :: String -> Maybe Int
day01part2 = fmap fst3 . find snd3 . scanl applyDelta (0, False, Set.empty) . cycle . getInput
  where
    applyDelta :: (Int, Bool, Set Int) -> Int -> (Int, Bool, Set Int)
    applyDelta (curr, _, seen) delta = let new = curr + delta
                                      in (new, Set.member new seen, Set.insert new seen)
