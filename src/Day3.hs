module Day3 where

import           Data.List            (find, group, sort)
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Claim = Claim {
  claimId  :: String
  , left   :: Int
  , top    :: Int
  , width  :: Int
  , height :: Int
  } deriving Show

type Parser = Parsec () String

claim :: Parser Claim
claim = Claim
        <$> (char '#' *> digitChar `manyTill` char ' ' <* string "@ ")
        <*> (read <$> digitChar `manyTill` char ',')
        <*> (read <$> digitChar `manyTill` char ':' <* char ' ')
        <*> (read <$> digitChar `manyTill` char 'x')
        <*> (read <$> digitChar `manyTill` newline)

day3part1 :: String -> String
day3part1 s =
  case parse (many claim) "" s of
    Left e       -> show e
    Right claims -> show $ length $ filter (\x -> length x > 1) $ group $ sort $ concatMap toCoords claims

toCoords :: Claim -> [(Int, Int)]
toCoords c = do
  x <- [left c..(left c + width c - 1)]
  y <- [top c..(top c + height c - 1)]
  return (x, y)

data Coord = Coord {
  cClaimId  :: String
  , cCoords :: [(Int, Int)]
  } deriving Show

day3part2 :: String -> String
day3part2 s =
  case parse (many claim) "" s of
    Left e       -> show e
    Right claims -> show $ f claims
  where
    f :: [Claim] -> Maybe Coord
    f claims =
      let
        cs = map (\c -> Coord (claimId c) (toCoords c)) claims
        coords = Set.fromList $ concat $ filter (\x -> length x == 1) $ group $ sort $ concatMap cCoords cs
      in
        find (isGoodClaim coords) cs

    isGoodClaim :: Set (Int, Int) -> Coord -> Bool
    isGoodClaim gs c = all (`Set.member` gs) (cCoords c)
