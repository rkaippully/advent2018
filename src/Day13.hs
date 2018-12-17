{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Day13 where

import           Control.Applicative ((<|>))
import           Control.Monad.State
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as M
import           Data.Maybe          (fromJust, fromMaybe)
import           Debug.Trace


data TrackDir = Vertical
              | Horizontal
              | ForwardSlash
              | BackwardSlash
              | Intersection
              deriving (Show, Eq)

data Cart = Cart {
  faceDir    :: FaceDir
  , nextTurn :: TurnDir
  } deriving (Show)

data FaceDir = FUp | FLeft | FDown | FRight
  deriving (Show)

data TurnDir = TLeft | TRight | TStraight
  deriving (Show)

nextTurnDir :: TurnDir -> TurnDir
nextTurnDir TLeft     = TStraight
nextTurnDir TStraight = TRight
nextTurnDir TRight    = TLeft

nextFaceDir :: FaceDir -> TurnDir -> FaceDir
nextFaceDir fDir TStraight = fDir
nextFaceDir FUp TLeft      = FLeft
nextFaceDir FLeft TLeft    = FDown
nextFaceDir FDown TLeft    = FRight
nextFaceDir FRight TLeft   = FUp
nextFaceDir FUp TRight     = FRight
nextFaceDir FLeft TRight   = FUp
nextFaceDir FDown TRight   = FLeft
nextFaceDir FRight TRight  = FDown

newtype X = X Int
  deriving (Eq, Ord, Num, Enum, Show)
newtype Y = Y Int
  deriving (Eq, Ord, Num, Enum, Show)

type TrackMap = Map (Y, X) TrackDir
type CartMap = Map (Y, X) Cart

data CartState = CartState {
  allCarts         :: CartMap
  , collisionPoint :: Maybe (Y, X)
  }

makeTracksAndCarts :: String -> (TrackMap, CartMap)
makeTracksAndCarts = foldr doLine (M.empty, M.empty) . zip [0..] . lines
  where
    doLine :: (Y, String) -> (TrackMap, CartMap) -> (TrackMap, CartMap)
    doLine (y, line) xs = foldr (doChar y) xs $ zip [0..] line

    doChar :: Y -> (X, Char) -> (TrackMap, CartMap) -> (TrackMap, CartMap)
    doChar _ (_, ' ') xs              = xs
    doChar y (x, c) (tracks, carts)   =
      let
        tracks' = (\dir -> M.insert (y, x) dir tracks) <$> charToTrackDir c
        carts' = (\dir -> M.insert (y, x) (Cart dir TLeft) carts) <$> charToFaceDir c
      in
        (fromMaybe tracks tracks', fromMaybe carts carts')

    charToTrackDir :: Char -> Maybe TrackDir
    charToTrackDir c = lookup c [('|', Vertical), ('-', Horizontal)
                                , ('/', ForwardSlash), ('\\', BackwardSlash)
                                , ('>', Horizontal), ('<', Horizontal)
                                , ('^', Vertical), ('v', Vertical)
                                , ('+', Intersection)]

    charToFaceDir :: Char -> Maybe FaceDir
    charToFaceDir c = lookup c [('^', FUp), ('v', FDown), ('<', FLeft), ('>', FRight)]

moveCarts :: TrackMap -> CartMap -> CartState
moveCarts tracks carts = M.foldlWithKey f (CartState carts Nothing) carts
  where
    f :: CartState -> (Y, X) -> Cart -> CartState
    f state (y, x) (Cart fDir tDir) =
      let
        crts' = M.delete (y, x) $ allCarts state
        (y', x') = case fDir of
                     FUp    -> (y-1, x)
                     FDown  -> (y+1, x)
                     FLeft  -> (y, x-1)
                     FRight -> (y, x+1)
        (fDir', tDir') = case (fromJust (M.lookup (y', x') tracks), fDir) of
                           (Vertical, _)           -> (fDir, tDir)
                           (Horizontal, _)         -> (fDir, tDir)
                           (ForwardSlash, FUp)     -> (FRight, tDir)
                           (ForwardSlash, FDown)   -> (FLeft, tDir)
                           (ForwardSlash, FLeft)   -> (FDown, tDir)
                           (ForwardSlash, FRight)  -> (FUp, tDir)
                           (BackwardSlash, FUp)    -> (FLeft, tDir)
                           (BackwardSlash, FDown)  -> (FRight, tDir)
                           (BackwardSlash, FLeft)  -> (FUp, tDir)
                           (BackwardSlash, FRight) -> (FDown, tDir)
                           (Intersection, _)       -> (nextFaceDir fDir tDir, nextTurnDir tDir)
      in
        if M.member (y', x') crts'
        then trace ("Collision at " ++ show y' ++ " " ++ show x') CartState{ allCarts = M.delete (y', x') crts'
                                                                           , collisionPoint = collisionPoint state <|> Just (y', x') }
        else state{ allCarts = M.insert (y', x') (Cart fDir' tDir') crts' }

findCollisionPoint :: TrackMap -> CartMap -> (Y, X)
findCollisionPoint tracks carts =
  let
    state = moveCarts tracks carts
  in
    fromMaybe (findCollisionPoint tracks $ allCarts state) (collisionPoint state)

day13part1 :: String -> (Int, Int)
day13part1 s =
  let
    (Y y, X x) = uncurry findCollisionPoint $ makeTracksAndCarts s
  in
    (x, y)

findLastCart :: TrackMap -> CartMap -> (Y, X)
findLastCart tracks carts =
  let
    carts' = allCarts $ moveCarts tracks carts
  in
    if M.size carts' == 1
    then head $ M.keys carts'
    else findLastCart tracks carts'

day13part2 :: String -> (Int, Int)
day13part2 s =
  let
    (Y y, X x) = uncurry findLastCart $ makeTracksAndCarts s
  in
    (x, y)
