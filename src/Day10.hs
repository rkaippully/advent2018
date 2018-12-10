module Day10 where

import           Control.Arrow ((&&&))
import           Data.List     (iterate, sort)
import           Util


data Point = Point {
  x, y     :: !Int
  , vx, vy :: !Int
  } deriving Show

instance Eq Point where
  p1 == p2 = x p1 == x p2 && y p1 == y p2

instance Ord Point where
  p1 `compare` p2 = (y p2, x p2) `compare` (y p1, x p1)

day10part1 :: String -> String
day10part1 = snd . findMessage

findMessage :: String -> (Int, String)
findMessage s =
  let
    (n, pts) = head $ dropWhile (not . isConverged) $ zip [0..] $ iterate move $ map toPoint $ lines s
    s' = printPoints $ sort pts
  in
    (n, s')

toPoint :: String -> Point
toPoint s =
  let
    _x = read $ sublist 10 16 s
    _y = read $ sublist 18 24 s
    _vx = read $ sublist 36 38 s
    _vy = read $ sublist 40 42 s
  in
    Point _x _y _vx _vy

move :: [Point] -> [Point]
move = map $ \p -> p {
  x = x p + vx p
  , y = y p + vy p
  }

isConverged :: (a, [Point]) -> Bool
isConverged (_, pts) =
  let
    (minX, maxX) = (minimum &&& maximum) $ map x pts
    (minY, maxY) = (minimum &&& maximum) $ map y pts
  in
    (maxX - minX) < 70 && (maxY - minY) < 70

printPoints :: [Point] -> String
printPoints pts = snd $ foldr f (pts, "") [(p, q) | q <- [(minY-3)..(maxY+3)], p <- [(minX-3)..(maxX+3)]]
  where
    (minX, maxX) = (minimum &&& maximum) $ map x pts
    (minY, maxY) = (minimum &&& maximum) $ map y pts

    f :: (Int, Int) -> ([Point], String) -> ([Point], String)
    f (x1, _) ([], s) =
      let s' = if x1 == (maxX+3) then '.':'\n':s else '.':s
      in ([], s')
    f (x1, y1) (p:ps, s) =
      let s' = if x1 == (maxX+3) then '\n':s else s
      in if (x1, y1) == (x p, y p) then (dropWhile (\t -> (x1, y1) == (x t, y t)) ps, '#':s') else (p:ps, '.':s')

day10part2 :: String -> Int
day10part2 = fst . findMessage
