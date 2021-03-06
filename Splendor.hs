{-# LANGUAGE RecordWildCards #-}

module Splendor where

import Prelude hiding (subtract)

import Control.Monad

import Data.List
import qualified Data.Set as Set
import Data.IntMap hiding (map, filter, foldr)
import Data.Monoid
import Debug.Trace

import Data

next p | p < 0 = -p
next p = p `mod` players + 1

evalBasic :: Int -> State -> Int
evalBasic p State{..} = score (hands ! p)

evalRelative :: Int -> State -> Int
evalRelative 1 State{..} = score (hands ! 1) - score (hands ! 2)
evalRelative 2 State{..} = score (hands ! 2) - score (hands ! 1)

evalCardBuy :: Int -> State -> Int
evalCardBuy p State{..} = buyCards
  where
    buyCards = Set.size $ Set.filter canBuy (Set.union onTable (reserved (hands ! p)))
    canBuy card = 0 == bagSize ((cost card `discount` cards (hands ! p)) `discount` coins (hands ! p))

--  100 winLoss + 1.5 Points + 2.5 Nobles + Prestige + Gems
-- Note - no nobles yet.
evalJoshua :: Int -> State -> Int
evalJoshua p State{..} = (if s >= 1600 then 1000 else if score (hands ! op) >= 1600 then minBound else 0) + (s `div` 100) * 20 + 10 * bagSize (cards (hands ! p)) + bagSize (coins (hands ! p))
  where
    s = score (hands ! p)
    op = if p == 1 then 2 else 1


gameOver State{..} = any (\Hand{..} -> score >= 1600) hands

emptyBag = GemBag 0 0 0 0 0 0

players :: Int
players = 2

initialCards = Set.fromList $ map fromNum [88, 32, 69, 89, 38, 21, 60, 85, 29, 82, 90, 35]
initialInDeck = Set.difference (Set.fromList $ map fromNum [10..99]) initialCards

graceHand = Hand (GemBag 2 2 1 1 1 1) (GemBag 0 0 1 1 1 0) 2 (Set.fromList [fromNum 39])
aiHand = Hand (GemBag 1 1 2 3 2 0) (GemBag 0 0 2 0 1 0) 103 (Set.fromList [fromNum 12])

initialState = State 1 (fromList [(1, aiHand), (2, graceHand)]) (GemBag 1 1 1 0 1 4) initialCards initialInDeck

emptyHand = Hand emptyBag emptyBag 0 Set.empty

{-# INLINE addToBag #-}
addToBag Diamond GemBag{..} = GemBag (diamond + 1) saphire emerald ruby onyx joker
addToBag Sapphire GemBag{..} = GemBag diamond (saphire + 1) emerald ruby onyx joker
addToBag Emerald GemBag{..} = GemBag diamond saphire (emerald + 1) ruby onyx joker
addToBag Ruby    GemBag{..} = GemBag diamond saphire emerald (ruby + 1) onyx joker
addToBag Onyx    GemBag{..} = GemBag diamond saphire emerald ruby (onyx + 1) joker
addToBag Joker   GemBag{..} = GemBag diamond saphire emerald ruby onyx (joker + 1)

{-# INLINE bagHas #-}
bagHas Diamond GemBag{..} = diamond >= 1
bagHas Sapphire GemBag{..} = saphire >= 1
bagHas Emerald GemBag{..} = emerald >= 1
bagHas Ruby    GemBag{..} = ruby >= 1
bagHas Onyx    GemBag{..} = onyx >= 1
bagHas Joker   GemBag{..} = joker >= 1

{-# INLINE discount #-}
discount cost by = GemBag (minZero $ diamond cost - diamond by)
                          (minZero $ saphire cost - saphire by)
                          (minZero $ emerald cost - emerald by)
                          (minZero $ ruby cost - ruby by)
                          (minZero $ onyx cost - onyx by)
                          (minZero $ joker cost - joker by)

subtract total cost | not $ canAfford total cost = error $ "Tried taking more than there is " ++ show total ++ ": " ++ show cost
subtract total cost | canAffordWithoutJokers total cost = discount total cost
subtract total cost = GemBag diamonds' saphires' emeralds' rubies' onyx' joker'
  where
    diamonds' = minZero $ diamond total - diamond cost
    saphires' = minZero $ saphire total - saphire cost
    emeralds' = minZero $ emerald total - emerald cost
    rubies' = minZero $ ruby total - ruby cost
    onyx' = minZero $ onyx total - onyx cost
    joker' = (-) (joker total) . getSum $ (Sum . deficit $ diamond total - diamond cost)
                                       <> (Sum . deficit $ saphire total - saphire cost)
                                       <> (Sum . deficit $ emerald total - emerald cost)
                                       <> (Sum . deficit $ ruby total - ruby cost)
                                       <> (Sum . deficit $ onyx total - onyx cost)
    deficit x | x > 0 = 0
    deficit x = -x

{-# INLINE bagSize #-}
bagSize GemBag{..} = diamond + saphire + emerald + ruby + onyx + joker

{-# INLINE minZero #-}
minZero = max 0

maxZero = min 0

plus :: GemBag -> GemBag -> GemBag
{-# INLINE plus #-}
plus a b = GemBag (diamond a + diamond b)
                  (saphire a + saphire b)
                  (emerald a + emerald b)
                  (ruby a + ruby b)
                  (onyx a + onyx b)
                  (joker a + joker b)

{-# INLINE canAfford #-}
{-# ANN canAfford "HLint: ignore" #-}
canAfford a b = (maxZero $ diamond a - diamond b) + (maxZero $ saphire a - saphire b) + (maxZero $ emerald a - emerald b) + (maxZero $ ruby a - ruby b) + (maxZero $ onyx a - onyx b) >= -(joker a)

canAffordWithoutJokers a b = (diamond a >= diamond b) && (saphire a >= saphire b) && (emerald a >= emerald b) && (ruby a >= ruby b) && (onyx a >= onyx b)

mkFirst state = state { player = 1, hands = fromList [(1, hands state ! 2), (2, hands state ! 1)]}

updateBank gem1 gem2 = GemBag ((4 - diamond gem1) - diamond gem2) ((4 - saphire gem1) - saphire gem2) ((4 - emerald gem1) - emerald gem2) ((4 - ruby gem1) - ruby gem2) ((4 - onyx gem1) - onyx gem2) ((5 - joker gem1) - joker gem2)

{-# ANN updateState "HLint: ignore Reduce duplication" #-}
updateState (Many l) state@State{..} = Prelude.foldr (\update (_, state) -> updateState update state) (if player == 1 then Max else Min, state) l
updateState Quit state = error "Quitting ..."
updateState (TakeTwo gem) State{..} = (if player == players then Max else Min, State (player `mod` players + 1) hands' bank' onTable remaining)
  where
    hands' = adjust tt player hands
    bank' = updateBank (coins (hands' ! 1)) (coins (hands' ! 2)) -- bank `subtract` gemBag
    tt Hand{..} = Hand (coins `plus` gemBag) cards score reserved
    gemBag = addToBag gem (addToBag gem emptyBag)
updateState (Take gs) State{..} = (if player == players then Max else Min, State (player `mod` players + 1) hands' bank' onTable remaining)
  where
    hands' = adjust tt player hands
    bank' = updateBank (coins (hands' ! 1)) (coins (hands' ! 2)) --bank `subtract` gemBag
    tt Hand{..} = Hand (coins `plus` gemBag) cards score reserved
    gemBag = Prelude.foldr addToBag emptyBag gs
updateState (BuyCard card) State{..} = (Chaos, State (-(player `mod` players + 1)) hands' bank' onTable' remaining)
  where
    onTable' = Set.delete (fromNum card) onTable
    hands' = adjust addCard player hands
    bank' = updateBank (coins (hands' ! 1)) (coins (hands' ! 2))--bank `plus` payment
    payment = cost (fromNum card) `discount` cards (hands ! player)
    addCard Hand{..} = Hand (coins `subtract` payment) (addToBag (gem (fromNum card)) cards) (score + (100 * points (fromNum card)) + 1) (Set.delete (fromNum card) reserved)
updateState (Reserve bool card) State{..} = (Chaos, State (-(player `mod` players + 1)) hands' bank' onTable' remaining)
  where
    hands' = adjust addReserved player hands
    addReserved hand = hand { reserved = Set.insert (fromNum card) (reserved hand), coins = if bool then addToBag Joker (coins hand) else coins hand }
    onTable' = Set.delete (fromNum card) onTable
    bank' = updateBank (coins (hands' ! 1)) (coins (hands' ! 2))
updateState (NewCard card) State{..} = (if abs player == 1 then Max else Min, State (if player < 0 then -player else player) hands bank onTable' remaining')
  where
    onTable' = Set.insert (fromNum card) onTable
    remaining' = Set.delete (fromNum card) remaining
updateState NoNewCard state = (if abs (player state) == 1 then Max else Min, state { player = if player state < 0 then -(player state) else player state })
updateState (Magic gems scoreNew cardsRemoved bankNew) State{..} = (if abs player == 1 then Max else Min, State (player `mod` players + 1) hands' bank' onTable' remaining)
  where
    onTable' = Set.filter (\x -> number x `notElem` cardsRemoved) onTable
    hands' = adjust addTo player hands
    addTo Hand{..} = Hand coins (foldr addToBag cards gems) (score + scoreNew) reserved
    bank' = bankNew

stateUpdate = flip updateState

--children s         | traceShow (hands s) False = undefined
children state@State{..} | player < 0 = updates
  where
    updates = [NewCard (number card) | card <- Set.toList remaining]
children State{..} = buyCards ++ threeCoins ++ if cs <= 8 then twoCoins else [] ++ if Set.size (reserved $ hands ! player) < 3 then if cs <= 9 && joker bank > 0 then reserveds else reservedNoJoke else []
  where
    cs = bagSize (coins (hands ! player))
    buyCards = map (BuyCard . number) . Set.toList $ Set.filter canBuy (Set.union onTable (reserved (hands ! player)))
    canBuy card = (cards (hands ! player) `plus` coins (hands ! player)) `canAfford` (cost card)
    --canBuy card = 0 == bagSize ((cost card `discount` cards (hands ! player)) `discount` coins (hands ! player))
    twoCoins = [TakeTwo Diamond | diamond bank >= 4]
               ++ [TakeTwo Sapphire | saphire bank >= 4]
               ++ [TakeTwo Emerald | emerald bank >= 4]
               ++ [TakeTwo Ruby | ruby bank >= 4]
               ++ [TakeTwo Onyx | onyx bank >= 4]
    threeCoins = map Take $ filter (all (`bagHas` bank) <&&> (\x -> length x + cs <= 10)) threeCoinSets
    reserveds = map (Reserve True . number) . Set.toList $ onTable
    reservedNoJoke = map (Reserve False . number) . Set.toList $ onTable

coinSet n = filter (\x -> length (nub x) == n)$ replicateM n [Diamond .. Onyx]
coinSet' = map coinSet [1..]
coinSetCumulative = scanl (++) [] coinSet'
threeCoinSets = reverse $ coinSetCumulative !! 3

a <&&> b = \x -> a x && b x
