module Day14 where

import           Data.List     (isPrefixOf, tails)
import           Data.Sequence (Seq)
import qualified Data.Sequence as S


day14part1 :: String -> [Int]
day14part1 s = take 10 $ drop (read s) recipes

recipes :: [Int]
recipes = 3 : 7 : go 0 1 (S.fromList [3, 7])
  where
    go :: Int -> Int -> Seq Int -> [Int]
    go idx1 idx2 sq = nextScores ++ go idx1' idx2' sq'
      where
        score1 = sq `S.index` idx1
        score2 = sq `S.index` idx2
        nextScores = toDigits (score1 + score2)
        sq' = sq <> S.fromList nextScores
        idx1' = (idx1+score1+1) `mod` S.length sq'
        idx2' = (idx2+score2+1) `mod` S.length sq'

toDigits :: Int -> [Int]
toDigits n | n < 10    = [n]
           | otherwise = let (n', r) = n `divMod` 10
                         in toDigits n' ++ [r]

day14part2 :: String -> Int
day14part2 s =
  let ns = toDigits $ read s
  in length $ takeWhile (not . (ns `isPrefixOf`)) $ tails recipes

{-
mkSequence :: (Seq Int -> Bool) -> Seq Int
mkSequence p = go (1, 0) $ S.fromList [7, 3]
  where
    go :: (Int, Int) -> Seq Int -> Seq Int
    go _ sq | p sq = sq
    go (idx1, idx2) sq =
      let (score1, score2) = (sq!?idx1, sq!?idx2)
      in case liftA2 (\a b -> divMod (a+b) 10) score1 score2 of
           Just (0, b) -> go (nextIndices (idx1+1) (idx2+1) score1 score2 (S.length sq + 1)) (b <| sq)
           Just (1, b) -> go (nextIndices (idx1+2) (idx2+2) score1 score2 (S.length sq + 2)) (b <| 1 <| sq)
           Just _      -> error "Invalid score"
           Nothing     -> error "Invalid index"

    nextIndices :: Int -> Int -> Maybe Int -> Maybe Int -> Int -> (Int, Int)
    nextIndices idx1 idx2 (Just score1) (Just score2) sz = ((idx1-score1-1) `mod` sz, (idx2-score2-1) `mod` sz)
    nextIndices _ _ _ _ _ = error "Invalid scores"
-}

{-
  let
    suffix = toDigitList (read s)
    n = length suffix
  in
    subtract n $ S.length $ mkSequence ((== suffix) . toList . S.take n)
-}
