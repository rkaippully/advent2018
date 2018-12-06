module Day3 where

import           Data.List            (groupBy, sortOn)
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Text.Megaparsec
import           Text.Megaparsec.Char


newtype ClaimId = ClaimId { claimNumber :: Int } deriving (Eq, Ord, Show)
newtype X = X Int deriving (Eq, Ord, Show)
newtype Y = Y Int deriving (Eq, Ord, Show)

data Claim = Claim {
  claimId  :: ClaimId
  , xCoord :: X
  , yCoord :: Y
  } deriving (Show)

coord :: Claim -> (X, Y)
coord (Claim _ x y) = (x, y)

equalCoord :: Claim -> Claim -> Bool
equalCoord x y = coord x == coord y

-- Claims grouped together by their coordinates
groupedClaims :: String -> [[Claim]]
groupedClaims = groupBy equalCoord . sortOn coord . getClaims

day3part1 :: String -> Int
day3part1 = length . filter (\x -> length x > 1) . groupedClaims

day3part2 :: String -> Int
day3part2 s =
  let
    pickClaimIds :: ([Claim] -> Bool) -> [[Claim]] -> Set ClaimId
    pickClaimIds f = Set.fromList . concatMap (map claimId) . filter f

    goodClaimIds = pickClaimIds (\x -> length x == 1) $ groupedClaims s
    badClaimIds = pickClaimIds (\x -> length x > 1) $ groupedClaims s
  in
    claimNumber $ Set.findMin $ Set.difference goodClaimIds badClaimIds

getClaims :: String -> [Claim]
getClaims s =
  case parse (many parseClaim) "" s of
    Left e   -> error $ show e
    Right xs -> concat xs

type Parser = Parsec () String

parseClaim :: Parser [Claim]
parseClaim = do
  cId <- read <$> (char '#' >> many digitChar)
  left <- read <$> (string " @ " >> many digitChar)
  top <- read <$> (char ',' >> many digitChar)
  width <- read <$> (string ": " >> many digitChar)
  height <- read <$> (char 'x' >> many digitChar)
  _ <- newline
  return [Claim (ClaimId cId) (X x) (Y y) | x <- [left..(left+width-1)], y <- [top..(top+height-1)]]
