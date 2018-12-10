{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}

module Main where

import           Day01              (day01part1, day01part2)
import           Day02              (day02part1, day02part2)
import           Day03              (day03part1, day03part2)
import           Day04              (day04part1, day04part2)
import           Day05              (day05part1, day05part2)
import           Day06              (day06part1, day06part2)
import           Day07              (day07part1, day07part2)
import           Day08              (day08part1, day08part2)
import           Day09              (day09part1, day09part2)
import           Day10              (day10part1, day10part2)
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

class ToString a where
  toString :: a -> String

instance ToString [Char] where
  toString = id

instance ToString Int where
  toString = show

instance ToString a => ToString (Maybe a) where
  toString Nothing  = "Nothing"
  toString (Just x) = "Just " ++ toString x

data Showable = forall a. ToString a => Showable a

showable :: ToString a => (String -> a) -> String -> Showable
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
  showable day09part1, showable day09part2,
  showable day10part1, showable day10part2
  ]

runAdvent :: Int -> Int -> IO ()
runAdvent d p = interact $ \s ->
  case (allParts !! ((d-1)*2+p-1)) s of
    Showable x -> toString x
