module Day05 where

import           Data.Char (toLower)

-- Remove newline
getInput :: String -> String
getInput = head . lines

day05part1 :: String -> Int
day05part1 = length . react . getInput

react :: String -> String
react = foldr reduce ""
  where
    reduce :: Char -> String -> String
    reduce c [] = [c]
    reduce c1 (c2:cs)
      | c1 /= c2 && toLower c1 == toLower c2 = cs
      | otherwise                            = c1:(c2:cs)

day05part2 :: String -> Int
day05part2 str = minimum $ map (findReduction $ getInput str) ['a'..'z']
  where
    findReduction :: String -> Char -> Int
    findReduction s delChar = length $ react $ filter (\c -> toLower c /= delChar) s
