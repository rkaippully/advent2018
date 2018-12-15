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
import           Day11              (day11part1, day11part2)
import           Day12              (day12part1, day12part2)
import           Day13              (day13part1, day13part2)
import           Day14              (day14part1, day14part2)
import           Day15              (day15part1, day15part2)
import           Day16              (day16part1, day16part2)
import           Day17              (day17part1, day17part2)
import           Day18              (day18part1, day18part2)
import           Day19              (day19part1, day19part2)
import           Day20              (day20part1, day20part2)
import           Day21              (day21part1, day21part2)
import           Day22              (day22part1, day22part2)
import           Day23              (day23part1, day23part2)
import           Day24              (day24part1, day24part2)
import           Day25              (day25part1, day25part2)
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

instance (ToString a, ToString b) => ToString (a, b) where
  toString (x, y) = "(" ++ toString x ++ ", " ++ toString y ++ ")"

instance (ToString a, ToString b, ToString c) => ToString (a, b, c) where
  toString (x, y, z) = "(" ++ toString x ++ ", " ++ toString y ++ ", " ++ toString z ++ ")"

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
  showable day10part1, showable day10part2,
  showable day11part1, showable day11part2,
  showable day12part1, showable day12part2,
  showable day13part1, showable day13part2,
  showable day14part1, showable day14part2,
  showable day15part1, showable day15part2,
  showable day16part1, showable day16part2,
  showable day17part1, showable day17part2,
  showable day18part1, showable day18part2,
  showable day19part1, showable day19part2,
  showable day20part1, showable day20part2,
  showable day21part1, showable day21part2,
  showable day22part1, showable day22part2,
  showable day23part1, showable day23part2,
  showable day24part1, showable day24part2,
  showable day25part1, showable day25part2
  ]

runAdvent :: Int -> Int -> IO ()
runAdvent d p = interact $ \s ->
  case (allParts !! ((d-1)*2+p-1)) s of
    Showable x -> toString x
