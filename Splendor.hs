{-# LANGUAGE RecordWildCards #-}

module Splendor where

import qualified Data.Set as Set
import Data.IntMap hiding (map)
import Debug.Trace

eval State{..} = score (hands ! 1) - score (hands ! 2)

gameOver State{..} = any (\Hand{..} -> score >= 15) hands

emptyBag = GemBag 0 0 0 0 0

data Gem = Diamond | Saphire | Emerald | Ruby | Onyx deriving (Eq, Show, Enum, Bounded, Ord)

players :: Int
players = 2

data State = State
  { player :: Int
  , hands :: IntMap Hand
  , bank :: GemBag
  , onTable :: Set.Set Card
  , remaining :: Set.Set Card
  } deriving (Eq, Show)

initialCards = Set.fromList $ map fromNum [82, 28, 84, 35, 19, 21, 85, 66, 17, 34, 74, 14]
initialInDeck = Set.difference (Set.fromList $ map fromNum [10..99]) initialCards
initialState = State 1 (fromList [(1, emptyHand), (2, emptyHand)]) (GemBag 4 4 4 4 4) initialCards initialInDeck

data Card = Card
  { points :: Int
  , gem :: Gem
  , cost :: GemBag -- hope this is faster than a generic bag
  } deriving (Eq, Show, Ord)

data GemBag = GemBag
  { diamond :: Int
  , saphire :: Int
  , emerald :: Int
  , ruby    :: Int
  , onyx    :: Int
  } deriving (Eq, Show, Ord)

data Hand = Hand
  { coins :: GemBag
  , cards :: GemBag
  , score :: Int
  } deriving (Eq, Show)

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

fromNum 14 = Card 0 Saphire (GemBag 1 0 0 0 2)
fromNum 74 = Card 0 Saphire (GemBag 0 1 3 1 0)
fromNum 34 = Card 0 Diamond (GemBag 0 3 0 0 0)
fromNum 17 = Card 0 Diamond (GemBag 3 1 0 0 1)
fromNum 32 = Card 0 Saphire (GemBag 1 0 1 2 1)
fromNum 37 = Card 0 Emerald (GemBag 1 1 0 1 2)
fromNum 27 = Card 0 Emerald (GemBag 0 2 0 2 0)
fromNum 22 = Card 1 Ruby    (GemBag 4 0 0 0 0)
fromNum 13 = Card 0 Ruby    (GemBag 2 1 1 0 1)
fromNum 70 = Card 0 Ruby    (GemBag 1 1 1 0 1)
fromNum 20 = Card 0 Saphire (GemBag 1 0 1 1 1)
fromNum 36 = Card 0 Saphire (GemBag 0 0 0 0 3)
fromNum 96 = Card 0 Saphire (GemBag 1 0 2 2 0)
fromNum 55 = Card 0 Emerald (GemBag 1 1 0 1 1)
fromNum 97 = Card 1 Emerald (GemBag 0 0 0 0 4)
fromNum 10 = Card 0 Emerald (GemBag 2 1 0 0 0)
fromNum 53 = Card 0 Onyx    (GemBag 2 2 0 1 0)
fromNum 89 = Card 0 Onyx    (GemBag 1 2 1 1 0)
fromNum 18 = Card 0 Onyx    (GemBag 2 0 2 0 0)
fromNum 50 = Card 0 Onyx    (GemBag 0 0 2 1 0)
fromNum 65 = Card 0 Onyx    (GemBag 0 0 1 3 1)
fromNum 40 = Card 0 Ruby    (GemBag 1 0 0 1 3)
fromNum 39 = Card 0 Emerald (GemBag 0 0 0 3 0)
fromNum 11 = Card 0 Diamond (GemBag 0 0 0 2 1)
fromNum 88 = Card 0 Saphire (GemBag 0 0 2 0 2)
fromNum 81 = Card 1 Diamond (GemBag 0 0 4 0 0)
fromNum 83 = Card 0 Diamond (GemBag 0 2 0 0 2)
fromNum 15 = Card 0 Diamond (GemBag 0 1 2 1 1)
fromNum 67 = Card 0 Diamond (GemBag 0 2 2 0 1)
fromNum 69 = Card 1 Saphire (GemBag 0 0 0 4 0)
fromNum 51 = Card 0 Ruby    (GemBag 3 0 0 0 0)
fromNum 23 = Card 0 Emerald (GemBag 1 3 1 0 0)
fromNum 48 = Card 0 Diamond (GemBag 0 1 1 1 1)
fromNum 99 = Card 0 Ruby    (GemBag 2 0 0 2 0)
fromNum 44 = Card 0 Ruby    (GemBag 0 2 1 0 0)
fromNum 24 = Card 0 Emerald (GemBag 0 1 0 2 2)
fromNum 52 = Card 0 Onyx    (GemBag 0 0 3 0 0)
fromNum 16 = Card 1 Onyx    (GemBag 0 4 0 0 0)
fromNum 64 = Card 0 Onyx    (GemBag 1 1 1 1 0)
fromNum 91 = Card 0 Ruby    (GemBag 2 0 1 0 2)
fromNum 85 = Card 1 Onyx    (GemBag 3 2 2 0 0)
fromNum 19 = Card 1 Diamond (GemBag 0 0 3 2 2)
fromNum 66 = Card 1 Saphire (GemBag 0 2 3 0 3)
fromNum 21 = Card 1 Emerald (GemBag 2 3 0 0 2)
fromNum 26 = Card 2 Ruby    (GemBag 0 0 0 0 5)
fromNum 41 = Card 1 Saphire (GemBag 0 2 2 3 0)
fromNum 98 = Card 3 Ruby    (GemBag 0 0 0 6 0)
fromNum 42 = Card 2 Onyx    (GemBag 0 1 4 2 0)
fromNum 47 = Card 2 Emerald (GemBag 0 5 3 0 0)
fromNum 79 = Card 1 Ruby    (GemBag 2 0 0 2 3)
fromNum 95 = Card 2 Saphire (GemBag 5 3 0 0 0)
fromNum 60 = Card 3 Saphire (GemBag 0 6 0 0 0)
fromNum 54 = Card 2 Onyx    (GemBag 0 0 5 3 0)
fromNum 77 = Card 1 Ruby    (GemBag 0 3 0 2 3)
fromNum 56 = Card 2 Emerald (GemBag 4 2 0 0 1)
fromNum 61 = Card 2 Emerald (GemBag 0 0 5 0 0)
fromNum 73 = Card 2 Onyx    (GemBag 5 0 0 0 0)
fromNum 71 = Card 2 Ruby    (GemBag 1 4 2 0 0)
fromNum 30 = Card 2 Diamond (GemBag 0 0 1 4 2)
fromNum 46 = Card 3 Diamond (GemBag 6 0 0 0 0)
fromNum 38 = Card 2 Diamond (GemBag 0 0 0 5 3)
fromNum 62 = Card 2 Diamond (GemBag 0 0 0 5 0)
fromNum 33 = Card 2 Ruby    (GemBag 3 0 0 0 5)
fromNum 75 = Card 2 Saphire (GemBag 0 5 0 0 0)
fromNum 43 = Card 1 Diamond (GemBag 2 3 0 3 0)
fromNum 86 = Card 3 Emerald (GemBag 0 0 6 0 0)
fromNum 76 = Card 2 Saphire (GemBag 2 0 0 1 4)
fromNum 12 = Card 1 Emerald (GemBag 3 0 2 3 0)
fromNum 57 = Card 3 Onyx    (GemBag 0 0 0 0 6)
fromNum 80 = Card 1 Onyx    (GemBag 3 0 3 0 2)
fromNum 35 = Card 3 Saphire (GemBag 3 0 3 3 5)
fromNum 82 = Card 4 Onyx    (GemBag 0 0 0 7 0)
fromNum 84 = Card 5 Onyx    (GemBag 0 0 0 7 3)
fromNum 28 = Card 4 Emerald (GemBag 0 7 0 0 0)
fromNum 93 = Card 4 Emerald (GemBag 3 6 3 0 0)
fromNum 94 = Card 4 Saphire (GemBag 6 3 0 0 3)
fromNum 87 = Card 3 Emerald (GemBag 5 3 0 3 3)
fromNum 72 = Card 4 Diamond (GemBag 3 0 0 3 6)
fromNum 45 = Card 5 Ruby    (GemBag 0 0 7 3 0)
fromNum 63 = Card 3 Ruby    (GemBag 3 5 3 0 3)
fromNum 31 = Card 4 Onyx    (GemBag 0 0 3 6 3)
fromNum 49 = Card 4 Diamond (GemBag 0 0 0 0 7)
fromNum 90 = Card 3 Diamond (GemBag 0 3 3 5 3)
fromNum 92 = Card 4 Ruby    (GemBag 0 3 6 3 0)
fromNum 78 = Card 5 Diamond (GemBag 3 0 0 0 7)
fromNum 58 = Card 3 Onyx    (GemBag 3 3 5 3 0)
fromNum 59 = Card 4 Saphire (GemBag 7 0 0 0 0)
fromNum 25 = Card 5 Saphire (GemBag 7 3 0 0 0)
fromNum 29 = Card 4 Ruby    (GemBag 0 0 7 0 0)
fromNum 68 = Card 5 Emerald (GemBag 0 7 3 0 0)
