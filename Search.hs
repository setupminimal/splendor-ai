{-# LANGUAGE BangPatterns #-}

module Search where

import Debug.Trace

import Splendor
import Data
--import Eval

dispatch player (Chaos, x) depth alpha beta = fst $ chaosNode player x depth alpha beta
dispatch player (Min, x) depth alpha beta = fst $ minNode player x depth alpha beta
dispatch player (Max, x) depth alpha beta = fst $ maxNode player x depth alpha beta

f >< g = \(x, y) -> (f x, g y)
dist f x = (f x, x)

maxNode :: (Num a, Ord a) => Int -> State -> a -> Int -> Int -> (Int, Maybe Edit)
maxNode player state depth alpha beta | depth <= 0 || gameOver state = (eval player state, trace "Max Node eval" Nothing)
maxNode player state depth alpha beta = if null ch
                                        then
                                          (alpha, Nothing)
                                        else
                                          go alpha (error "Max Node - Go initial")
                                          . map ((dispatch player >< Just)
                                          . dist (stateUpdate state)) $ ch
  where
    ch = children state
    go a ct [] = (a, ct)
    go a ct _ | beta <= a = (a, ct)
    go a ct ((st, action):sts) = go (a `max` rem) (if a > rem then ct else action) sts
      where
        rem = st (depth - 1) a beta


minNode player state depth alpha beta | depth <= 0 || gameOver state = (eval player state, trace "Min Node eval" Nothing)
minNode player state depth alpha beta = if null ch
                                        then
                                          (beta, Nothing)
                                        else
                                          go beta (error "Min Node - Go initial")
                                          . map ((dispatch player >< Just)
                                          . dist (stateUpdate state)) $ ch
  where
    ch = children state
    go b ct [] = (b, ct)
    go b ct _ | b <= alpha = (b, ct)
    go b ct ((st, action):sts) = go (b `min` rem) (if b < rem then ct else action) sts
      where
        rem = st (depth - 1) alpha b

chaosNode player state depth alpha beta | depth <= 0 || gameOver state = (eval player state, Nothing)
chaosNode player state depth alpha beta = (mean 0 0 . map (apply . dispatch player . stateUpdate state) $ children state, trace "Chaos Node - No action" Nothing)
  where
    apply st = st (depth - 1) alpha beta

mean :: (Integral t, Bounded t) => t -> t -> [t] -> t
mean 0    _    [] = minBound
mean !len !sum [] = sum `div` len
mean !len !sum (x:xs) = mean (len + 1) (sum + x) xs

