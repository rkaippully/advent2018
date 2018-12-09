{-# LANGUAGE TupleSections #-}

module Day07 where

import           Data.Bifunctor      (bimap, first)
import           Data.Char           (ord)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.List           (iterate, unfoldr)
import           Data.Maybe          (fromMaybe, isJust)
import           Data.Set            (Set)
import qualified Data.Set            as S


type Graph = HashMap Char (Set Char)

day07part1 :: String -> String
day07part1 = unfoldr doNextWork . makeGraph

makeGraph :: String -> Graph
makeGraph = makeMap . map getEdge . lines

getEdge :: String -> (Char,  Char)
getEdge s = (s!!36, s!!5)

makeMap :: [(Char, Char)] -> HashMap Char (Set Char)
makeMap = foldl (\m (from, to) -> HM.alter (Just . S.insert to . fromMaybe S.empty) from m) def
  where
    def = HM.fromList $ map (, S.empty) ['A'..'Z']

pickNextStep :: Graph -> Maybe (Char, Graph)
pickNextStep g = case HM.keys $ HM.filter null g of
                   [] -> Nothing
                   cs -> let c = minimum cs in Just (c, removeStep c g)

doNextWork :: Graph -> Maybe (Char, Graph)
doNextWork g = do
  (c, g') <- pickNextStep g
  return (c, stepComplete c g')

removeStep :: Char -> Graph -> Graph
removeStep = HM.delete

stepComplete :: Char -> Graph -> Graph
stepComplete c = HM.map (S.delete c)


type State = ([Maybe (Char, Int)], Graph)

day07part2 :: String -> Int
day07part2 s =
  length $ takeWhile isWorking $ iterate doNextStep startState
  where
    g = makeGraph s :: Graph
    startState = doNextStep (replicate 5 Nothing, g) :: State

isWorking :: State -> Bool
isWorking (workers, _) = any isJust workers

doNextStep :: State -> State
doNextStep = assignWork . clearFinishedSteps

clearFinishedSteps :: State -> State
clearFinishedSteps ([], g)                  = ([], g)
clearFinishedSteps (Just (c, 1):workers, g) = bimap (Nothing :) (stepComplete c) $ clearFinishedSteps (workers, g)
clearFinishedSteps (worker:workers, g)      = first (worker :) $ clearFinishedSteps (workers, g)

assignWork :: State -> State
assignWork ([], g)                  = ([], g)
assignWork (Just (c, n):workers, g) = first (Just (c, n-1) :) $ assignWork (workers, g)
assignWork (Nothing:workers, g)     = case pickNextStep g of
                                        Nothing      -> first (Nothing :) $ assignWork (workers, g)
                                        Just (c, g') -> first (Just (c, ord c - ord 'A' + 61) :) $ assignWork (workers, g')
