{-# LANGUAGE ExistentialQuantification #-}

module Main where

import           Day01
import           Day02
import           Day03
import           Day04
import           Day05
import           Day06
import           Day07
import           Day08
import           Day09
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
  showable day01part1, showable day01part2,
  showable day02part1, showable day02part2,
  showable day03part1, showable day03part2,
  showable day04part1, showable day04part2,
  showable day05part1, showable day05part2,
  showable day06part1, showable day06part2,
  showable day07part1, showable day07part2,
  showable day08part1, showable day08part2,
  showable day09part1, showable day09part2
  ]

runAdvent :: Int -> Int -> IO ()
runAdvent d p = interact $ \s ->
  case (allParts !! ((d-1)*2+p-1)) s of
    Showable x -> show x
