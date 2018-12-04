{-# LANGUAGE ExistentialQuantification #-}

module Main where

import           Day1
import           Day2
import           Day3
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

data Showable = forall a. Show a => Showable a

showable :: Show a => (String -> a) -> String -> Showable
showable f = Showable . f

allParts :: [String -> Showable]
allParts = [
  showable day1part1, showable day1part2,
  showable day2part1, showable day2part2,
  showable day3part1, showable day3part2
  ]

runAdvent :: Int -> Int -> IO ()
runAdvent d p = interact $ \s ->
  case (allParts !! ((d-1)*2+p-1)) s of
    Showable x -> show x
