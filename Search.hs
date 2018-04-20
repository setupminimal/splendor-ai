{-# LANGUAGE BangPatterns #-}

module Search where

import Debug.Trace

import Splendor
import Data
--import Eval

dispatch prev eval player (Chaos, x) depth alpha beta = fst $ gammaChaosNode prev eval player x depth alpha beta
dispatch prev eval player (Min, x) depth alpha beta = fst $ minNode prev eval player x depth alpha beta
dispatch prev eval player (Max, x) depth alpha beta = fst $ maxNode prev eval player x depth alpha beta

f >< g = \(x, y) -> (f x, g y)
dist f x = (f x, x)


nonPruningMaxNode _ eval player state depth alpha beta | depth <= 0 || gameOver state = (eval player state, Just Win)
nonPruningMaxNode _ eval player state depth alpha beta = maximum . map explore $ children state
  where
    explore move = (dispatch Max eval player (updateState move state) (depth - 1) alpha beta, Just move)

nonPruningMinNode _ eval player state depth alpha beta | depth <= 0 || gameOver state = (eval player state, error "Beta last move.")
nonPruningMinNode _ eval player state depth alpha beta = minimum . map explore $ children state
  where
    explore move = (dispatch Min eval player (updateState move state) (depth - 1) alpha beta, Just move)


maxNode :: (Num a, Ord a) => Next -> (Int -> State -> Int) -> Int -> State -> a -> Int -> Int -> (Int, Maybe Edit)
maxNode _ eval player state depth alpha beta | depth <= 0 || gameOver state = (eval player state, Just Win)
maxNode _ eval player state depth alpha beta = if null ch
                                        then
                                          (alpha, Nothing)
                                        else
                                          go alpha Nothing
                                          . map ((dispatch Max eval player >< Just)
                                                 . dist (stateUpdate state)) $ ch
  where
    ch = children state
    go a ct [] = (a, ct)
    go a ct _ | beta <= a = (a, ct)
    go a Nothing ((st, action):sts) | st (depth - 1) a beta >= a = go (st (depth - 1) a beta) action sts
    go a ct ((st, action):sts) = go (a `max` rem) (if a >= rem then ct else action) sts
      where
        rem = st (depth - 1) a beta

gammaChaosNode _ eval player state depth alpha beta | depth <= 0 || gameOver state = (eval player state, Just Win)
gammaChaosNode Max eval player state depth alpha beta = if null ch
                                                      then
                                                        (alpha, Nothing)
                                                      else
                                                        go alpha (300000*length ch)
                                                        . map ((dispatch Chaos eval player >< Just)
                                                              . dist (stateUpdate state)) $ ch
  where
    ch = children state
    go alpha bestCase [] = (alpha `max` (bestCase `div` length ch), Nothing)
    go alpha bestCase (move:moves) | bestCase `div` length ch < alpha = (alpha, Nothing)
    go alpha bestCase ((st, action):moves) = go (alpha `max` rem) (bestCase - 300000 + rem) moves
      where
        rem = st (depth - 1) alpha beta
gammaChaosNode Min eval player state depth alpha beta = if null ch
                                                      then
                                                        (alpha, Nothing)
                                                      else
                                                        go beta 0
                                                        . map ((dispatch Chaos eval player >< Just)
                                                              . dist (stateUpdate state)) $ ch
  where
    ch = children state
    go beta worstCase [] = (beta `min` (worstCase `div` length ch), Nothing)
    go beta worstCase (move:moves) | worstCase `div` length ch > beta = (beta, Nothing)
    go beta worstCase ((st, action):moves) = go (beta `min` rem) (worstCase + rem) moves
      where
        rem = st (depth - 1) alpha beta


minNode _ eval player state depth alpha beta | depth <= 0 || gameOver state = (eval player state, trace "Min Node eval" Nothing)
minNode _ eval player state depth alpha beta = if null ch
                                        then
                                          (beta, Nothing)
                                        else
                                          go beta Nothing
                                          . map ((dispatch Min eval player >< Just)
                                                 . dist (stateUpdate state)) $ ch
  where
    ch = children state
    go b ct [] = (b, ct)
    go b ct _ | b <= alpha = (b, ct)
    go b ct ((st, action):sts) = go (b `min` rem) (if b <= rem then ct else action) sts
      where
        rem = st (depth - 1) alpha b

chaosNode _ eval player state depth alpha beta | depth <= 0 || gameOver state = (eval player state, Nothing)
chaosNode _ eval player state depth alpha beta = (mean 0 0 . map (apply . dispatch Chaos eval player . stateUpdate state) $ children state, trace "Chaos Node - No action" Nothing)
  where
    apply st = st (depth - 1) alpha beta

stochasticChaosNode _ eval player state depth alpha beta | depth <= 0 || gameOver state = (eval player state, Nothing)
stochasticChaosNode _ eval player state depth alpha beta = (mean 0 0 . map (apply . dispatch Chaos eval player . stateUpdate state) . take 5 $ children state, trace "Chaos Node - No action" Nothing)
  where
    apply st = st (depth - 1) alpha beta

mean :: (Integral t, Bounded t) => t -> t -> [t] -> t
mean 0    _    [] = minBound
mean !len !sum [] = sum `div` len
mean !len !sum (x:xs) = mean (len + 1) (sum + x) xs

