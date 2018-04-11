{-# LANGUAGE BangPatterns #-}

module Search where

import Debug.Trace

import Splendor
import Data
--import Eval

dispatch eval player (Chaos, x) depth alpha beta = fst $ chaosNode eval player x depth alpha beta
dispatch eval player (Min, x) depth alpha beta = fst $ minNode eval player x depth alpha beta
dispatch eval player (Max, x) depth alpha beta = fst $ maxNode eval player x depth alpha beta

f >< g = \(x, y) -> (f x, g y)
dist f x = (f x, x)

maxNode :: (Num a, Ord a) => (Int -> State -> Int) -> Int -> State -> a -> Int -> Int -> (Int, Maybe Edit)
maxNode eval player state depth alpha beta | depth <= 0 || gameOver state = (eval player state, Just Win)
maxNode eval player state depth alpha beta = if null ch
                                        then
                                          (alpha, Nothing)
                                        else
                                          go alpha Nothing
                                          . map ((dispatch eval player >< Just)
                                          . dist (stateUpdate state)) $ ch
  where
    ch = children state
    go a ct [] = (a, ct)
    go a ct _ | beta <= a = (a, ct)
    go a Nothing ((st, action):sts) | st (depth - 1) a beta >= a = go (st (depth - 1) a beta) action sts
    go a ct ((st, action):sts) = go (a `max` rem) (if a >= rem then ct else action) sts
      where
        rem = st (depth - 1) a beta


minNode eval player state depth alpha beta | depth <= 0 || gameOver state = (eval player state, trace "Min Node eval" Nothing)
minNode eval player state depth alpha beta = if null ch
                                        then
                                          (beta, Nothing)
                                        else
                                          go beta Nothing
                                          . map ((dispatch eval player >< Just)
                                          . dist (stateUpdate state)) $ ch
  where
    ch = children state
    go b ct [] = (b, ct)
    go b ct _ | b <= alpha = (b, ct)
    go b ct ((st, action):sts) = go (b `min` rem) (if b <= rem then ct else action) sts
      where
        rem = st (depth - 1) alpha b

chaosNode eval player state depth alpha beta | depth <= 0 || gameOver state = (eval player state, Nothing)
chaosNode eval player state depth alpha beta = (mean 0 0 . map (apply . dispatch eval player . stateUpdate state) $ children state, trace "Chaos Node - No action" Nothing)
  where
    apply st = st (depth - 1) alpha beta

mean :: (Integral t, Bounded t) => t -> t -> [t] -> t
mean 0    _    [] = minBound
mean !len !sum [] = sum `div` len
mean !len !sum (x:xs) = mean (len + 1) (sum + x) xs

