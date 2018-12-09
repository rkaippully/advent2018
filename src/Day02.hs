module Day02 where

import           Data.List  (find, group, sort)
import           Data.Maybe (isJust)
import           Data.Set   (Set)
import qualified Data.Set   as Set


day02part1 :: String -> Int
day02part1 ids =
  let
    charLengths = map length . group . sort -- lengths of each chars in a line
    ls = map charLengths (lines ids)
    twos = length $ filter id $ elem 2 <$> ls
    threes = length $ filter id $ elem 3 <$> ls
  in
    twos * threes

day02part2 :: String -> Maybe String
day02part2 ids =
  let
    mkPairs s = [(take n s, drop (n+1) s) | n <- [0..length s]]
    allPairs = concatMap mkPairs $ lines ids

    checkSeen :: (Maybe String, Set (String, String)) -> (String, String) -> (Maybe String, Set (String, String))
    checkSeen (_, seen) x@(prefix, suffix) = let dup = if Set.member x seen
                                                       then Just (prefix ++ suffix)
                                                       else Nothing
                                             in (dup, Set.insert x seen)
  in
    find (isJust . fst) (scanl checkSeen (Nothing, Set.empty) allPairs) >>= fst
