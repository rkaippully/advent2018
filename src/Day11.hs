module Day11 where

import           Data.Array (Array, (!))
import qualified Data.Array as A
import           Data.List  (maximumBy)
import           Data.Ord   (comparing)
import           Util


type Serial = Int
type SummedAreaTable = Array (Int, Int) Int

day11part1 :: String -> (Int, Int)
day11part1 s =
  let
    table = summedAreaTable (read s)
  in
    first $ maximumBy (comparing second) [((x, y), powerOfArea table x y 3) | x <- [1..298], y <- [1..298]]

powerOfArea :: SummedAreaTable -> Int -> Int -> Int -> Int
powerOfArea table x y size = table!(x+size-1, y+size-1) + table!(x-1, y-1) - table!(x+size-1, y-1) - table!(x-1, y+size-1)

--
-- https://en.wikipedia.org/wiki/Summed-area_table
--
summedAreaTable :: Serial -> SummedAreaTable
summedAreaTable serial = sums
  where
    sums = A.array ((0, 0), (300, 300)) [((x, y), sumOf x y) | x <- [0..300], y <- [0..300]]

    sumOf :: Int -> Int -> Int
    sumOf x y | x == 0 || y == 0 = 0
              | otherwise        = powerOf x y + sums!(x, y-1) + sums!(x-1, y) - sums!(x-1, y-1)

    powerOf :: Int -> Int -> Int
    powerOf x y =
      let
        rack = x + 10
        power = (rack * y + serial) * rack
        hundred = (power `div` 100) `mod` 10
      in
        hundred - 5

day11part2 :: String -> (Int, Int, Int)
day11part2 s =
  let
    table = summedAreaTable (read s)
  in
    first $ maximumBy (comparing snd) [((x, y, size), powerOfArea table x y size) | size <- [1..300], x <- [1..(301-size)], y <- [1..(301-size)]]
