{-# LANGUAGE TypeFamilies #-}

module Util where


class Tuple2 a where
  type First a
  type Second a
  first :: a -> First a
  second :: a -> Second a

class Tuple2 a => Tuple3 a where
  type Third a
  third :: a -> Third a

class Tuple3 a => Tuple4 a where
  type Fourth a
  fourth :: a -> Fourth a

instance Tuple2 (a, b) where
  type First (a, b)  = a
  type Second (a, b) = b
  first (a, _)  = a
  second (_, b) = b

instance Tuple2 (a, b, c) where
  type First (a, b, c)  = a
  type Second (a, b, c) = b
  first (a, _, _)  = a
  second (_, b, _) = b

instance Tuple3 (a, b, c) where
  type Third (a, b, c)  = c
  third (_, _, c)  = c

instance Tuple2 (a, b, c, d) where
  type First (a, b, c, d)  = a
  type Second (a, b, c, d) = b
  first (a, _, _, _)  = a
  second (_, b, _, _) = b

instance Tuple3 (a, b, c, d) where
  type Third (a, b, c, d)  = c
  third (_, _, c, _)  = c

instance Tuple4 (a, b, c, d) where
  type Fourth (a, b, c, d) = d
  fourth (_, _, _, d) = d

sublist :: Int -> Int -> [a] -> [a]
sublist from to = take (to - from) . drop from
