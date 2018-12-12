{-# LANGUAGE TypeFamilies #-}

module Util where


class Tuple a where
  type First a
  type Second a
  type Third a
  type Fourth a

  first :: a -> First a
  second :: a -> Second a
  third :: a -> Third a
  fourth :: a -> Fourth a

instance Tuple (a, b) where
  type First (a, b)  = a
  type Second (a, b) = b

  first (a, _)  = a
  second (_, b) = b

instance Tuple (a, b, c) where
  type First (a, b, c)  = a
  type Second (a, b, c) = b
  type Third (a, b, c)  = c

  first (a, _, _)  = a
  second (_, b, _) = b
  third (_, _, c)  = c

instance Tuple (a, b, c, d) where
  type First (a, b, c, d)  = a
  type Second (a, b, c, d) = b
  type Third (a, b, c, d)  = c
  type Fourth (a, b, c, d) = d

  first (a, _, _, _)  = a
  second (_, b, _, _) = b
  third (_, _, c, _)  = c
  fourth (_, _, _, d) = d

sublist :: Int -> Int -> [a] -> [a]
sublist from to = take (to - from) . drop from
