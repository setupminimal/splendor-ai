module Data where

import Data.Set
import Data.IntMap

data Gem = Diamond | Saphire | Emerald | Ruby | Onyx deriving (Eq, Show, Enum, Bounded, Ord)

data State = State
  { player :: Int
  , hands :: IntMap Hand
  , bank :: GemBag
  , onTable :: Set Card
  , remaining :: Set Card
  } deriving (Eq, Show)

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

