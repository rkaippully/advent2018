module Day04 where

import           Control.Arrow       ((&&&))
import           Data.Char           (isDigit)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.List           (isPrefixOf, maximumBy, sort)
import           Data.Maybe          (fromJust, fromMaybe)
import           Data.Ord            (comparing)
import           Util


type GuardId = Int
type Time = Int
type Freq = Int

data Event = ShiftBegin GuardId
           | FallsAsleep Time
           | WakesUp Time
           deriving Show

type SleepState = HashMap GuardId (Time, HashMap Time Freq)

getTime :: String -> Int
getTime = read . sublist 15 17

toEvent :: String -> Event
toEvent s
  | "Guard" `isPrefixOf` eventStr = ShiftBegin $ read $ takeWhile isDigit $ drop 7 eventStr
  | "falls" `isPrefixOf` eventStr = FallsAsleep t
  | "wakes" `isPrefixOf` eventStr = WakesUp t
  | otherwise                     = error $ "Bad input, Santa: " ++ s
  where
    eventStr = drop 19 s
    t = getTime s

toSleeps :: (GuardId, Time, SleepState) -> Event -> (GuardId, Time, SleepState)
toSleeps (_, t, slps) (ShiftBegin g) = (g, t, slps)
toSleeps (g, _, slps) (FallsAsleep t) = (g, t, slps)
toSleeps (g, t, slps) (WakesUp t') = (g, t, slps')
  where
    slps' = let
              (oldTime, oldFreq) = fromMaybe (0, HM.empty) $ HM.lookup g slps
              newFreq = foldl incFreq oldFreq [t..(t'-1)]
              incFreq m tm = HM.alter (Just . (+ 1) . fromMaybe 0) tm m
            in
              HM.insert g (oldTime + t' - t, newFreq) slps

getSleeps :: String -> SleepState
getSleeps = third . foldl toSleeps (undefined, undefined, HM.empty) . map toEvent . sort . lines

day04part1 :: String -> Int
day04part1 input =
  let
    slps = getSleeps input
    guardId = fst $ maximumBy (comparing $ fst . snd) $ HM.toList slps
    mostSleepyTime = fst $ maximumBy (comparing snd) $ HM.toList $ snd $ fromJust $ HM.lookup guardId slps
  in
    guardId * mostSleepyTime

day04part2 :: String -> GuardId
day04part2 input =
  let
    slps = getSleeps input

    toFreq :: (GuardId, (Time, HashMap Time Freq)) -> [(GuardId, Time, Freq)]
    toFreq (g, (_, m)) = map (\(tm, freq) -> (g, tm, freq)) $ HM.toList m

    freqs :: [(GuardId, Time, Freq)]
    freqs = concatMap toFreq $ HM.toList slps

    (guardId, minute) = first &&& second $ maximumBy (comparing third) freqs
  in
    guardId * minute
