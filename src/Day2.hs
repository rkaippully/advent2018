module Day2 where

import           Control.Arrow  ((&&&))
import           Data.Bifunctor (bimap)
import           Data.List      (find, group, sort)


type Has2 = Bool
type Has3 = Bool

day2part1 :: String -> String
day2part1 = show . uncurry (*) . bimap countTrues countTrues . threesAndTwos
  where
    threesAndTwos :: String -> ([Has2], [Has3])
    threesAndTwos = unzip . map ((elem 2 &&& elem 3) . map length . group . sort) . lines

    countTrues :: [Bool] -> Int
    countTrues = length . filter (== True)

day2part2 :: String -> String
day2part2 s = maybe "" snd (correctBox s)
  where
    correctBox :: String -> Maybe (Bool, String)
    correctBox = find fst . map findCorrectBox . makePairs . lines

    makePairs :: [String] -> [(String, String)]
    makePairs []       = []
    makePairs (l : ls) = [ (l, l') | l' <- ls ] ++ makePairs ls

    -- Given a pair of strings, return the common chars between them
    findCommon :: String -> String -> String
    findCommon cs cs' = map fst . filter (uncurry (==)) $ zip cs cs'

    -- Given a pair of strings, return (b, s) where b is a boolean indicating  whether
    -- these are correct box IDs and s is the common characters between them.
    findCorrectBox :: (String, String) -> (Bool, String)
    findCorrectBox (cs, cs') =
      let s = findCommon cs cs' in (length cs == length s + 1, s)
