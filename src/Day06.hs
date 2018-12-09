module Day06 where

import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as HM
import           Data.List            (group, maximumBy, nub, sort, sortOn)
import           Data.Maybe           (catMaybes, mapMaybe)
import           Data.Ord             (comparing)
import           Text.Megaparsec
import           Text.Megaparsec.Char


day06part1 :: String -> Int
day06part1 s =
  let
    coords = getCoords s
    maxX = fst $ maximumBy (comparing fst) coords
    maxY = snd $ maximumBy (comparing snd) coords
    -- finites = filter (\(x, y) -> x /= minX && x /= maxX && y /= minY && y /= maxY) coords
    grid = [(x, y) | x <- [0..(maxX+1)], y <- [0..(maxY+1)]]

    -- Closest coordinate for each (x, y) point
    closest :: HashMap (Int, Int) (Maybe (Int, Int))
    closest = foldl (\m (x, y) -> HM.insert (x, y) (findClosest (x, y) coords) m) HM.empty grid

    edges =  [(x, y) | x <- [0..(maxX+1)], y <- [0, maxY+1]] ++ [(x, y) | x <- [0, maxX+1], y <- [0..(maxY+1)]]

    -- Coordinates with inifinite span
    infinites :: [(Int, Int)]
    infinites = catMaybes $ nub $ mapMaybe (`HM.lookup` closest) edges
  in
    maximum $ map length $ group $ sort $ filter (not . (`elem` infinites)) $ catMaybes $ HM.elems closest

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1, y1) (x2, y2) = abs(x1 - x2) + abs(y1 - y2)

findClosest :: (Int, Int) -> [(Int, Int)] -> Maybe (Int, Int)
findClosest point coords =
  case sortOn snd $ map (\p -> (p, distance point p)) coords of
    ((p1, d1):(_, d2):_) | d1 /= d2 -> Just p1
    _                               -> Nothing

getCoords :: String -> [(Int, Int)]
getCoords s =
  case parse (many parseCoord) "" s of
    Left e   -> error $ show e
    Right xs -> xs

type Parser = Parsec () String

parseCoord :: Parser (Int, Int)
parseCoord = do
  x <- read <$> many digitChar
  _ <- char ','
  _ <- space
  y <- read <$> many digitChar
  _ <- newline
  return (x, y)

day06part2 :: String -> Int
day06part2 s =
  let
    coords = getCoords s
    maxX = fst $ maximumBy (comparing fst) coords
    maxY = snd $ maximumBy (comparing snd) coords
    grid = [(x, y) | x <- [-100..(maxX+100)], y <- [-100..(maxY+100)]]

    distances :: (Int, Int) -> [Int]
    distances p = map (`distance` p) coords

    goodRegion :: [(Int, Int)]
    goodRegion = filter (\p -> sum (distances p) < 10000) grid
  in
    length goodRegion
