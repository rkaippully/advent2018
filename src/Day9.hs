module Day9 where

import           Data.List     (foldl')
import           Data.Sequence (Seq, ViewL (..), (><))
import qualified Data.Sequence as S


type Marble = Int
type Player = Int

data GameState = GameState {
  marbles        :: Seq Marble
  , currentIndex :: Int
  , scores       :: Seq Int
  } deriving Show

startState :: Player -> GameState
startState nPlayers = GameState {
  marbles = S.singleton 0
  , currentIndex = 0
  , scores = S.replicate nPlayers 0
  }

day9part1 :: String -> Int
day9part1 s =
  let
    [nPlayers, nMarbles] = read <$> words s
  in
    runGame nPlayers nMarbles

runGame :: Player -> Marble -> Int
runGame nPlayers nMarbles = maximum $ scores $ foldl' play (startState nPlayers) $ zip [1..nMarbles] (cycle [1..nPlayers])

play :: GameState -> (Marble, Player) -> GameState
play s (marble, player) | (marble `mod` 23) == 0 = specialPlay s (marble, player)
                        | otherwise              = normalPlay s marble

normalPlay :: GameState -> Marble -> GameState
normalPlay s marble =
  let
    n = length $ marbles s
    idx = (currentIndex s + 2) `mod` n
  in
    s {
      marbles = S.insertAt idx marble (marbles s)
      , currentIndex = idx
      }

specialPlay :: GameState -> (Marble, Player) -> GameState
specialPlay s (marble, player) =
  let
    n = length $ marbles s
    idx = (currentIndex s - 7) `mod` n
    (h, t) = S.splitAt idx (marbles s)
    sc = scores s
  in
    case S.viewl t of
      S.EmptyL -> s { marbles = h
                    , currentIndex = idx `mod` (n-1)
                    , scores = S.adjust' (+ marble) (player-1) sc }
      x:<xs    -> s { marbles = h >< xs
                    , currentIndex = idx `mod` (n-1)
                    , scores = S.adjust' (+ (marble + x)) (player-1) sc }

day9part2 :: String -> Int
day9part2 s =
  let
    [nPlayers, nMarbles] = read <$> words s
  in
    runGame nPlayers (nMarbles*100)
