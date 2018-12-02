module Main where

import           Day1
import           System.Environment (getArgs)
import           System.Exit        (die)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [day, part] -> runAdvent (read day) (read part)
    _           -> usage

usage :: IO ()
usage = die "Usage: advent2018 <day number> <part number>"

allParts :: [String -> String]
allParts = [
  day1part1, day1part2
  ]

runAdvent :: Int -> Int -> IO ()
runAdvent d p = interact $ allParts !! ((d-1)*2+p-1)
