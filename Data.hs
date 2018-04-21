{-# LANGUAGE RecordWildCards #-}

module Data where

import Data.Set hiding (map, fromList)
import Data.IntMap hiding (map, findMin)
import Data.Char (isDigit)

data Edit = TakeTwo Gem
  | Take [Gem]
  | BuyCard Int
  | NewCard Int
  | NoNewCard
  | Quit
  | Many [Edit]
  | Magic [Gem] Int [Int] -- Gems, Score, and Cards-removed-from-table
  | Reserve Bool Int
  | Win
  deriving (Eq, Show, Read, Ord)

data Next = Min | Max | Chaos deriving (Eq, Show)

data Gem = Diamond | Saphire | Emerald | Ruby | Onyx | Joker deriving (Eq, Show, Enum, Bounded, Ord, Read)

data State = State
  { player :: Int
  , hands :: IntMap Hand
  , bank :: GemBag
  , onTable :: Set Card
  , remaining :: Set Card
  } deriving (Eq, Read)

instance Show State where
  show State{..} = "State:\n  player: " ++ show player ++ "\n  hands: " ++ show hands ++ "\n  bank: " ++ show bank ++ "\n  onTable: " ++ show onTable ++ "\n  nextUp: " ++ show (findMin remaining)

data Card = Card
  { points :: Int
  , gem :: Gem
  , cost :: GemBag -- hope this is faster than a generic bag
  , number :: Int
  } deriving (Eq)

instance Ord Card where
  a `compare` b = number a `compare` number b

data GemBag = GemBag
  { diamond :: !Int
  , saphire :: !Int
  , emerald :: !Int
  , ruby    :: !Int
  , onyx    :: !Int
  , joker   :: !Int
  } deriving (Eq, Ord, Read)

instance Show GemBag where
    show GemBag{..} = show diamond ++ "D " ++ show saphire ++ "S " ++ show emerald ++ "E " ++ show ruby ++ "R " ++ show onyx ++ "O " ++ show joker ++ "J"

data Hand = Hand
  { coins :: GemBag
  , cards :: GemBag
  , score :: Int
  , reserved :: Set Card
  } deriving (Eq, Read)

instance Show Hand where
  show Hand{..} = "\nHand:\n    coins: " ++ show coins ++ "\n    cards: " ++ show cards ++ "\n    score: " ++ show score

fromNum' 14 = Card 0 Saphire (GemBag 1 0 0 0 2 0) 14
fromNum' 74 = Card 0 Saphire (GemBag 0 1 3 1 0 0) 74
fromNum' 34 = Card 0 Diamond (GemBag 0 3 0 0 0 0) 34
fromNum' 17 = Card 0 Diamond (GemBag 3 1 0 0 1 0) 17
fromNum' 32 = Card 0 Saphire (GemBag 1 0 1 2 1 0) 32
fromNum' 37 = Card 0 Emerald (GemBag 1 1 0 1 2 0) 37
fromNum' 27 = Card 0 Emerald (GemBag 0 2 0 2 0 0) 27
fromNum' 22 = Card 1 Ruby    (GemBag 4 0 0 0 0 0) 22
fromNum' 13 = Card 0 Ruby    (GemBag 2 1 1 0 1 0) 13
fromNum' 70 = Card 0 Ruby    (GemBag 1 1 1 0 1 0) 70
fromNum' 20 = Card 0 Saphire (GemBag 1 0 1 1 1 0) 20
fromNum' 36 = Card 0 Saphire (GemBag 0 0 0 0 3 0) 36
fromNum' 96 = Card 0 Saphire (GemBag 1 0 2 2 0 0) 96
fromNum' 55 = Card 0 Emerald (GemBag 1 1 0 1 1 0) 55
fromNum' 97 = Card 1 Emerald (GemBag 0 0 0 0 4 0) 97
fromNum' 10 = Card 0 Emerald (GemBag 2 1 0 0 0 0) 10
fromNum' 53 = Card 0 Onyx    (GemBag 2 2 0 1 0 0) 53
fromNum' 89 = Card 0 Onyx    (GemBag 1 2 1 1 0 0) 89
fromNum' 18 = Card 0 Onyx    (GemBag 2 0 2 0 0 0) 18
fromNum' 50 = Card 0 Onyx    (GemBag 0 0 2 1 0 0) 50
fromNum' 65 = Card 0 Onyx    (GemBag 0 0 1 3 1 0) 65
fromNum' 40 = Card 0 Ruby    (GemBag 1 0 0 1 3 0) 40
fromNum' 39 = Card 0 Emerald (GemBag 0 0 0 3 0 0) 39
fromNum' 11 = Card 0 Diamond (GemBag 0 0 0 2 1 0) 11
fromNum' 88 = Card 0 Saphire (GemBag 0 0 2 0 2 0) 88
fromNum' 81 = Card 1 Diamond (GemBag 0 0 4 0 0 0) 81
fromNum' 83 = Card 0 Diamond (GemBag 0 2 0 0 2 0) 83
fromNum' 15 = Card 0 Diamond (GemBag 0 1 2 1 1 0) 15
fromNum' 67 = Card 0 Diamond (GemBag 0 2 2 0 1 0) 67
fromNum' 69 = Card 1 Saphire (GemBag 0 0 0 4 0 0) 69
fromNum' 51 = Card 0 Ruby    (GemBag 3 0 0 0 0 0) 51
fromNum' 23 = Card 0 Emerald (GemBag 1 3 1 0 0 0) 23
fromNum' 48 = Card 0 Diamond (GemBag 0 1 1 1 1 0) 48
fromNum' 99 = Card 0 Ruby    (GemBag 2 0 0 2 0 0) 99
fromNum' 44 = Card 0 Ruby    (GemBag 0 2 1 0 0 0) 44
fromNum' 24 = Card 0 Emerald (GemBag 0 1 0 2 2 0) 24
fromNum' 52 = Card 0 Onyx    (GemBag 0 0 3 0 0 0) 52
fromNum' 16 = Card 1 Onyx    (GemBag 0 4 0 0 0 0) 16
fromNum' 64 = Card 0 Onyx    (GemBag 1 1 1 1 0 0) 64
fromNum' 91 = Card 0 Ruby    (GemBag 2 0 1 0 2 0) 91
fromNum' 85 = Card 1 Onyx    (GemBag 3 2 2 0 0 0) 85
fromNum' 19 = Card 1 Diamond (GemBag 0 0 3 2 2 0) 19
fromNum' 66 = Card 1 Saphire (GemBag 0 2 3 0 3 0) 66
fromNum' 21 = Card 1 Emerald (GemBag 2 3 0 0 2 0) 21
fromNum' 26 = Card 2 Ruby    (GemBag 0 0 0 0 5 0) 26
fromNum' 41 = Card 1 Saphire (GemBag 0 2 2 3 0 0) 41
fromNum' 98 = Card 3 Ruby    (GemBag 0 0 0 6 0 0) 98
fromNum' 42 = Card 2 Onyx    (GemBag 0 1 4 2 0 0) 42
fromNum' 47 = Card 2 Emerald (GemBag 0 5 3 0 0 0) 47
fromNum' 79 = Card 1 Ruby    (GemBag 2 0 0 2 3 0) 79
fromNum' 95 = Card 2 Saphire (GemBag 5 3 0 0 0 0) 95
fromNum' 60 = Card 3 Saphire (GemBag 0 6 0 0 0 0) 60
fromNum' 54 = Card 2 Onyx    (GemBag 0 0 5 3 0 0) 54
fromNum' 77 = Card 1 Ruby    (GemBag 0 3 0 2 3 0) 77
fromNum' 56 = Card 2 Emerald (GemBag 4 2 0 0 1 0) 56
fromNum' 61 = Card 2 Emerald (GemBag 0 0 5 0 0 0) 61
fromNum' 73 = Card 2 Onyx    (GemBag 5 0 0 0 0 0) 73
fromNum' 71 = Card 2 Ruby    (GemBag 1 4 2 0 0 0) 71
fromNum' 30 = Card 2 Diamond (GemBag 0 0 1 4 2 0) 30
fromNum' 46 = Card 3 Diamond (GemBag 6 0 0 0 0 0) 46
fromNum' 38 = Card 2 Diamond (GemBag 0 0 0 5 3 0) 38
fromNum' 62 = Card 2 Diamond (GemBag 0 0 0 5 0 0) 62
fromNum' 33 = Card 2 Ruby    (GemBag 3 0 0 0 5 0) 33
fromNum' 75 = Card 2 Saphire (GemBag 0 5 0 0 0 0) 75
fromNum' 43 = Card 1 Diamond (GemBag 2 3 0 3 0 0) 43
fromNum' 86 = Card 3 Emerald (GemBag 0 0 6 0 0 0) 86
fromNum' 76 = Card 2 Saphire (GemBag 2 0 0 1 4 0) 76
fromNum' 12 = Card 1 Emerald (GemBag 3 0 2 3 0 0) 12
fromNum' 57 = Card 3 Onyx    (GemBag 0 0 0 0 6 0) 57
fromNum' 80 = Card 1 Onyx    (GemBag 3 0 3 0 2 0) 80
fromNum' 35 = Card 3 Saphire (GemBag 3 0 3 3 5 0) 35
fromNum' 82 = Card 4 Onyx    (GemBag 0 0 0 7 0 0) 82
fromNum' 84 = Card 5 Onyx    (GemBag 0 0 0 7 3 0) 84
fromNum' 28 = Card 4 Emerald (GemBag 0 7 0 0 0 0) 28
fromNum' 93 = Card 4 Emerald (GemBag 3 6 3 0 0 0) 93
fromNum' 94 = Card 4 Saphire (GemBag 6 3 0 0 3 0) 94
fromNum' 87 = Card 3 Emerald (GemBag 5 3 0 3 3 0) 87
fromNum' 72 = Card 4 Diamond (GemBag 3 0 0 3 6 0) 72
fromNum' 45 = Card 5 Ruby    (GemBag 0 0 7 3 0 0) 45
fromNum' 63 = Card 3 Ruby    (GemBag 3 5 3 0 3 0) 63
fromNum' 31 = Card 4 Onyx    (GemBag 0 0 3 6 3 0) 31
fromNum' 49 = Card 4 Diamond (GemBag 0 0 0 0 7 0) 49
fromNum' 90 = Card 3 Diamond (GemBag 0 3 3 5 3 0) 90
fromNum' 92 = Card 4 Ruby    (GemBag 0 3 6 3 0 0) 92
fromNum' 78 = Card 5 Diamond (GemBag 3 0 0 0 7 0) 78
fromNum' 58 = Card 3 Onyx    (GemBag 3 3 5 3 0 0) 58
fromNum' 59 = Card 4 Saphire (GemBag 7 0 0 0 0 0) 59
fromNum' 25 = Card 5 Saphire (GemBag 7 3 0 0 0 0) 25
fromNum' 29 = Card 4 Ruby    (GemBag 0 0 7 0 0 0) 29
fromNum' 68 = Card 5 Emerald (GemBag 0 7 3 0 0 0) 68

cardNums = fromList numAssoc

fromNum x = cardNums ! x

numAssoc = map (\x -> (x, fromNum' x)) [10..99]

instance Show Card where
  show c = "Card " ++ show (number c)

fromJust (Just x) = x

instance Read Card where
  readsPrec _ x = let (n, rest) = span isDigit x in [(fromNum $ read n, rest)]
