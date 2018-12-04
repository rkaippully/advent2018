module Day1 where

import           Data.List  (find)
import           Data.Maybe (fromJust, fromMaybe)
import           Data.Set   (Set)
import qualified Data.Set   as Set
import           Util


getInput :: String -> [Int]
getInput = map readSignedInt . lines

readSignedInt :: String -> Int
readSignedInt ('+':cs) = read cs
readSignedInt cs       = read cs

day1part1 :: String -> Int
day1part1 = sum . getInput

day1part2 :: String -> Maybe Int
day1part2 = fmap fst3 . find snd3 . scanl applyDelta (0, False, Set.empty) . cycle . getInput
  where
    applyDelta :: (Int, Bool, Set Int) -> Int -> (Int, Bool, Set Int)
    applyDelta (curr, _, seen) delta = let new = curr + delta
                                      in (new, Set.member new seen, Set.insert new seen)
