module Day1 where

import           Data.List   (find)
import           Data.Maybe  (fromJust, fromMaybe)
import           Data.Set    (Set)
import qualified Data.Set    as Set
import           Debug.Trace (trace)


day1part1 :: String -> String
day1part1 = show . sum . map readSignedInt . lines

readSignedInt :: String -> Int
readSignedInt ('+':cs) = read cs
readSignedInt cs       = read cs

type Freq = Int

data FreqChanges = FreqChanges {
  -- ^ current frequency
  currFreq :: Maybe Freq
  -- ^ set of all frequencies seen so far
  , freqs  :: Set Freq
  } deriving Show

day1part2 :: String -> String
day1part2 = show . currFreq . fromJust . find isRepeat . scanl applyFreq start . cycle . map readSignedInt . lines
  where
    start :: FreqChanges
    start = FreqChanges Nothing Set.empty

    applyFreq :: FreqChanges -> Int -> FreqChanges
    applyFreq fq change =
      case currFreq fq of
        Nothing -> fq{currFreq = Just change}
        Just f  -> FreqChanges{currFreq = Just (f + change), freqs = Set.insert f (freqs fq)}

    isRepeat :: FreqChanges -> Bool
    isRepeat (FreqChanges Nothing _ )  = False
    isRepeat (FreqChanges (Just f) fs) = Set.member f fs
