{-# LANGUAGE RecordWildCards #-}

module Splendor where

import Control.Monad

import qualified Data.Set as Set
import Data.IntMap hiding (map, filter, foldr)
import Debug.Trace

import Data

{-# INLINE eval #-}
eval State{..} = score (hands ! 1) -- score (hands ! 2)

gameOver State{..} = any (\Hand{..} -> score >= 15) hands

emptyBag = GemBag 0 0 0 0 0

players :: Int
players = 2

initialCards = Set.fromList $ map fromNum [35, 82, 84, 28, 85, 19, 66, 21, 14, 74, 27, 17]
initialInDeck = Set.difference (Set.fromList $ map fromNum [10..99]) initialCards
initialState = State 1 (fromList [(1, emptyHand), (2, emptyHand)]) (GemBag 4 4 4 4 4) initialCards initialInDeck

emptyHand = Hand emptyBag emptyBag 0 Set.empty

{-# INLINE addToBag #-}
addToBag Diamond GemBag{..} = GemBag (diamond + 1) saphire emerald ruby onyx
addToBag Saphire GemBag{..} = GemBag diamond (saphire + 1) emerald ruby onyx
addToBag Emerald GemBag{..} = GemBag diamond saphire (emerald + 1) ruby onyx
addToBag Ruby    GemBag{..} = GemBag diamond saphire emerald (ruby + 1) onyx
addToBag Onyx    GemBag{..} = GemBag diamond saphire emerald ruby (onyx + 1)

{-# INLINE bagHas #-}
bagHas Diamond GemBag{..} = diamond >= 1
bagHas Saphire GemBag{..} = saphire >= 1
bagHas Emerald GemBag{..} = emerald >= 1
bagHas Ruby    GemBag{..} = ruby >= 1
bagHas Onyx    GemBag{..} = onyx >= 1

{-# INLINE discount #-}
discount cost by = GemBag (minZero $ diamond cost - diamond by)
                          (minZero $ saphire cost - saphire by)
                          (minZero $ emerald cost - emerald by)
                          (minZero $ ruby cost - ruby by)
                          (minZero $ onyx cost - onyx by)

{-# INLINE bagSize #-}
bagSize GemBag{..} = diamond + saphire + emerald + ruby + onyx

{-# INLINE minZero #-}
minZero a | a < 0 = 0
minZero a = a

{-# INLINE plus #-}
plus a b = GemBag (diamond a + diamond b)
                  (saphire a + saphire b)
                  (emerald a + emerald b)
                  (ruby a + ruby b)
                  (onyx a + onyx b)

{-# INLINE canAfford #-}
canAfford a b = (diamond a >= diamond b) && (saphire a >= saphire b) && (emerald a >= emerald b) && (ruby a >= ruby b) && (onyx a >= onyx b)

updateState (Many l) state@State{..} = Prelude.foldr (\update (_, state) -> updateState update state) (if player == 1 then Max else Min, state) l
updateState Quit state = error "Quitting ..."
updateState (TakeTwo gem) State{..} = (if player == players then Max else Min, State (player `mod` players + 1) hands' bank' onTable remaining)
  where
    hands' = adjust tt player hands
    bank' = bank `discount` gemBag
    tt Hand{..} = Hand (coins `plus` gemBag) cards score reserved
    gemBag = addToBag gem (addToBag gem emptyBag)
updateState (TakeThree g1 g2 g3) State{..} = (if player == players then Max else Min, State (player `mod` players + 1) hands' bank' onTable remaining)
  where
    hands' = adjust tt player hands
    bank' = bank `discount` gemBag
    tt Hand{..} = Hand (coins `plus` gemBag) cards score reserved
    gemBag = addToBag g1 (addToBag g2 (addToBag g3 emptyBag))
updateState (BuyCard card) State{..} = (Chaos, State (-(player `mod` players + 1)) hands' bank' onTable' remaining)
  where
    onTable' = Set.delete (fromNum card) onTable
    hands' = adjust addCard player hands
    bank' = bank `plus` payment
    payment = cost (fromNum card) `discount` cards (hands ! player)
    addCard Hand{..} = Hand (coins `discount` payment) (addToBag (gem (fromNum card)) cards) (score + (100 * points (fromNum card)) + 10) reserved
updateState (NewCard card) State{..} = (if abs player == 1 then Max else Min, State (if player < 0 then -player else player) hands bank onTable' remaining')
  where
    onTable' = Set.insert (fromNum card) onTable
    remaining' = Set.delete (fromNum card) remaining
updateState (Magic gems scoreNew) State{..} = (if abs player == 1 then Max else Min, State (player `mod` players + 1) hands' bank onTable remaining)
  where
    hands' = adjust addTo player hands
    addTo Hand{..} = Hand coins (foldr addToBag cards gems) (score + scoreNew) reserved

stateUpdate = flip updateState

--children s         | traceShow (hands s) False = undefined
children state@State{..} | player < 0 = updates
  where
    updates = [NewCard (number card) | card <- Set.toList remaining]
children State{..} = buyCards ++ if cs <= 7 then threeCoins else [] ++ if cs <= 8 then twoCoins else []
  where
    cs = bagSize (coins (hands ! player))
    buyCards = map (\c -> BuyCard (number c)) . Set.toList $ Set.filter canBuy onTable
    canBuy card = 0 == bagSize ( cost card `discount` (cards (hands ! player)) `discount` (coins (hands ! player)))
    twoCoins = [TakeTwo Diamond | diamond bank >= 4]
               ++ [TakeTwo Saphire | saphire bank >= 4]
               ++ [TakeTwo Emerald | emerald bank >= 4]
               ++ [TakeTwo Ruby | ruby bank >= 4]
               ++ [TakeTwo Onyx | onyx bank >= 4]
    threeCoins = map (uncurry3 TakeThree) $ filter (\(a, b, c) -> bagHas a bank && bagHas b bank && bagHas c bank) threeCoinSets


threeCoinSets = [(g1, g2, g3) | g1 <- [Diamond .. Onyx]
                              , g2 <- [Diamond .. Onyx]
                              , g3 <- [Diamond .. Onyx]
                              , g1 /= g2
                              , g1 /= g3
                              , g2 /= g3]

uncurry3 f (a, b, c) = f a b c
