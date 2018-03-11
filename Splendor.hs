{-# LANGUAGE RecordWildCards #-}

module Splendor where

import qualified Data.Set as Set
import Data.IntMap hiding (map)
import Debug.Trace

import Data
import CardInfo

eval State{..} = score (hands ! 1) - score (hands ! 2)

gameOver State{..} = any (\Hand{..} -> score >= 15) hands

emptyBag = GemBag 0 0 0 0 0

players :: Int
players = 2

initialCards = Set.fromList $ map fromNum [82, 28, 84, 35, 19, 21, 85, 66, 17, 34, 74, 14]
initialInDeck = Set.difference (Set.fromList $ map fromNum [10..99]) initialCards
initialState = State 1 (fromList [(1, emptyHand), (2, emptyHand)]) (GemBag 4 4 4 4 4) initialCards initialInDeck

emptyHand = Hand emptyBag emptyBag 0

addToBag Diamond GemBag{..} = GemBag (diamond + 1) saphire emerald ruby onyx
addToBag Saphire GemBag{..} = GemBag diamond (saphire + 1) emerald ruby onyx
addToBag Emerald GemBag{..} = GemBag diamond saphire (emerald + 1) ruby onyx
addToBag Ruby    GemBag{..} = GemBag diamond saphire emerald (ruby + 1) onyx
addToBag Onyx    GemBag{..} = GemBag diamond saphire emerald ruby (onyx + 1)

discount cost by = GemBag (minZero $ diamond cost - diamond by)
                          (minZero $ saphire cost - saphire by)
                          (minZero $ emerald cost - emerald by)
                          (minZero $ ruby cost - ruby by)
                          (minZero $ onyx cost - onyx by)

minZero a | a < 0 = 0
minZero a = a

plus a b = GemBag (diamond a + diamond b)
                  (saphire a + saphire b)
                  (emerald a + emerald b)
                  (ruby a + ruby b)
                  (onyx a + onyx b)

canAfford a b = (diamond a >= diamond b) && (saphire a >= saphire b) && (emerald a >= emerald b) && (ruby a >= ruby b) && (onyx a >= onyx b)


