{-# LANGUAGE RecordWildCards, BangPatterns #-}

module Search where

import Debug.Trace
import qualified Data.Set as Set
import Data.IntMap hiding (map)

import Splendor
import Data

maxNode state depth alpha beta | depth <= 0 || gameOver state = eval state
maxNode state depth alpha beta = go alpha (children state)
  where
    go a [] = a
    go a _ | beta <= a = a
    go a (st:sts) = go (a `max` rem) sts
      where
        rem = st (depth - 1) a beta


minNode state depth alpha beta | depth <= 0 || gameOver state = eval state
minNode state depth alpha beta = go beta (children state)
  where
    go b [] = b
    go b _ | b <= alpha = b
    go b (st:sts) = go (b `min` rem) sts
      where
        rem = st (depth - 1) alpha b

chaosNode state depth alpha beta | depth <= 0 || gameOver state = eval state
chaosNode state depth alpha beta = mean 0 0 . map apply $ children state
  where
    apply st = st (depth - 1) alpha beta

mean 0    !sum [] = minBound
mean !len !sum [] = sum `div` len
mean !len !sum (x:xs) = mean (len + 1) (sum + x) xs

-- Note: This is in the Search module only so that it can access the node functions

--children s         | traceShow (hands s) False = undefined
children State{..} | player < 0 = map chaosNode $ states
  where
    states = [State (-player) hands bank (Set.insert card onTable) (Set.delete card remaining) | card <- Set.toList remaining]
children State{..} = (map chaosNode buyCards) ++ (map (if player == players then maxNode else minNode) getCoins)
  where
    nextPlayer = player `mod` players + 1
    buyCards = [ let
                   addCard Hand{..} = Hand (remPayment coins) (addToBag (gem card) cards) (score + (100 * points card) + 1)
                   remPayment coins = coins `discount` payment
                   addPayment bank = bank `plus` payment
                   payment = cost card `discount` (cards (hands ! player))
                 in
                   State (-nextPlayer) (adjust addCard player hands) (addPayment bank) (Set.delete card onTable) remaining
               | card <- Set.toList onTable
               , (coins (hands ! player)) `canAfford` (cost card `discount` (cards (hands ! player)))
               ]
    getCoins = map f $ threeCoins ++ twoCoins
      where
        f grab = State nextPlayer (adjust addTake player hands) (bank `discount` grab) onTable remaining
          where
            addTake Hand{..} = Hand (coins `plus` grab) cards score
    twoCoins = (if diamond bank >= 4 then [GemBag 2 0 0 0 0] else [])
               ++ (if saphire bank >= 4 then [GemBag 0 2 0 0 0] else [])
               ++ (if emerald bank >= 4 then [GemBag 0 0 2 0 0] else [])
               ++ (if ruby bank >= 4 then [GemBag 0 0 0 2 0] else [])
               ++ (if onyx bank >= 4 then [GemBag 0 0 0 0 2] else [])
    threeCoins = [ addToBag g1 (addToBag g2 (addToBag g3 emptyBag))
                 | g1 <- [Diamond .. Onyx], g2 <- [Diamond .. Onyx], g3 <- [Diamond .. Onyx], g1 /= g2, g1 /= g3, g2 /= g3]

