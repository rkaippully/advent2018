module Day8 where

import           Data.Foldable (fold)
import           Data.List     (splitAt)
import           Data.Tree     (Tree (..))


getInput :: String -> [Int]
getInput = map read . words

type NodeId = Char
type Metadata = [Int]

type Count = Int

toTree :: [Int] -> Tree Metadata
toTree = fst . toTree'

toTree' :: [Int] -> (Tree Metadata, [Int])
toTree' (x:y:xs) = (Node meta children, rest)
  where
    childrenCount = x
    metaCount = y
    (children, xs') = getChildren childrenCount xs
    (meta, rest) = splitAt metaCount xs'
toTree' _ = error "Malformed tree"

getChildren :: Count -> [Int] -> ([Tree Metadata], [Int])
getChildren 0 xs = ([], xs)
getChildren n xs = let (t, xs') = toTree' xs
                       (ts, xs'') = getChildren (n-1) xs'
                   in (t:ts, xs'')

day8part1 :: String -> Int
day8part1 = sum . fold . toTree . getInput

day8part2 :: String -> Int
day8part2 = rootLabel . xformTree . toTree . getInput
  where
    xformTree :: Tree Metadata -> Tree Int
    xformTree (Node ms []) = Node (sum ms) []
    xformTree (Node ms ts) =
      let
        n = length ts
        ts' = foldr (\idx acc -> if idx > n then acc else xformTree (ts!!(idx-1)):acc) [] ms
        s = foldr (\(Node v _) acc -> v + acc) 0 ts'
      in
        Node s ts'
