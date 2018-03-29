{-# LANGUAGE BangPatterns #-}

module Search where

import Splendor
import Data

import Debug.Trace

dispatch (Chaos, x) depth alpha beta = fst $ chaosNode x depth alpha beta
dispatch (Min, x) depth alpha beta = fst $ minNode x depth alpha beta
dispatch (Max, x) depth alpha beta = fst $ maxNode x depth alpha beta

f >< g = \(x, y) -> (f x, g y)
dist f x = (f x, x)

maxNode :: (Num a, Ord a) => State -> a -> Int -> Int -> (Int, Maybe Edit)
maxNode state depth alpha beta | depth <= 0 || gameOver state = (eval state, Nothing)
maxNode state depth alpha beta = value
  where
    value = if null ch then (minBound, Nothing) else go alpha Nothing . map ((dispatch >< Just) . dist (stateUpdate state)) $ ch
    ch = children state
    go a ct [] = (a, ct)
    go a ct _ | beta <= a = (a, ct)
    go a ct ((st, action):sts) = go (a `max` rem) (if a > rem then ct else action) sts
      where
        rem = st (depth - 1) a beta


minNode state depth alpha beta | depth <= 0 || gameOver state = (eval state, Nothing)
minNode state depth alpha beta = if null ch then (minBound, Nothing) else go beta Nothing . map ((dispatch >< Just) . dist (stateUpdate state)) $ ch
  where
    ch = children state
    go b ct [] = (b, ct)
    go b ct _ | b <= alpha = (b, ct)
    go b ct ((st, action):sts) = go (b `min` rem) (if b < rem then ct else action) sts
      where
        rem = st (depth - 1) alpha b

chaosNode state depth alpha beta | depth <= 0 || gameOver state = (eval state, Nothing)
chaosNode state depth alpha beta = (mean 0 0 . map (apply . dispatch . stateUpdate state) $ children state, Nothing)
  where
    apply st = st (depth - 1) alpha beta

mean :: (Integral t, Bounded t) => t -> t -> [t] -> t
mean 0    _    [] = minBound
mean !len !sum [] = sum `div` len
mean !len !sum (x:xs) = mean (len + 1) (sum + x) xs

