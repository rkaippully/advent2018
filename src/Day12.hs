module Day12 where

import           Control.Comonad
import           Data.List       (intercalate)
import           Util


type HasPlant = Bool
type Index = Int
type Rule = [HasPlant]

data Zipper a = Zipper [a] a [a]

left :: Zipper a -> Zipper a
left (Zipper (l:ls) x rs) = Zipper ls l (x:rs)

right :: Zipper a -> Zipper a
right (Zipper ls x (r:rs)) = Zipper (x:ls) r rs

-- Window of n elements on both sides
window :: Int -> Zipper a -> [a]
window n (Zipper ls x rs) = reverse (take n ls) ++ [x] ++ take n rs

instance Functor Zipper where
  fmap f (Zipper ls x rs) = Zipper (fmap f ls) (f x) (fmap f rs)

instance Comonad Zipper where
  extract (Zipper _ x _) = x
  duplicate z = Zipper (tail $ iterate left z) z (tail $ iterate right z)

initialState :: String -> Zipper (Index, HasPlant)
initialState s = Zipper (zip [-1,-2..] (repeat False)) (head positives) (tail positives)
  where
    positives = zip [0..] (map (== '#') $ s ++ repeat '.')

day12part1 :: String -> Int
day12part1 s =
  let
    x:xs = lines s
    rules = makeRules xs
  in
    plantSum $ (!!20) $ iterate (=>> evolve rules) $ initialState x

makeRules :: [String] -> [Rule]
makeRules = map (map (== '#') . sublist 0 5) . filter (\s -> s!!9 == '#')

evolve :: [Rule] -> Zipper (Index, HasPlant) -> (Index, HasPlant)
evolve rules plants =
  let
    n = first $ extract plants
  in
    (n, map second (window 2 plants) `elem` rules)

plantSum :: Zipper (Index, HasPlant) -> Int
plantSum (Zipper ls x rs) = sum $ map first $ filter second $ take 1000 ls ++ [x] ++ take 1000 rs

toString :: [(Index, HasPlant)] -> String
toString = foldr (\(_, b) s -> (if b then '#' else '.') : s) ""

--
-- For my input, there is a pattern. plantSum goes up by 20 every generation after gen 111.
---
day12part2 :: String -> Int
day12part2 s =
  let
    x:xs = lines s
    rules = makeRules xs
    base = plantSum $ (!!111) $ iterate (=>> evolve rules) $ initialState x
  in
    (50000000000 - 111) * 20 + base
