module Main where

import qualified Data.Set as Set
import System.Random

import Search
import Splendor
import Data

depth = 4 -- fast turns

main :: IO ()
main = mainloop 1 initialState

mainloop p state = do
  let (escore, ed) = maxNode p state depth minBound maxBound
  putStrLn $ "Player " ++ show p ++ ": " ++ show ed ++ "\t\t(" ++ show escore ++ ")"
  if ed == Nothing then putStrLn "Done"
    else
    let (next, state') = updateState (fromJust ed) state in
      case next of
        Chaos -> handleChaos state' >>= mainloop (if p == 1 then 2 else 1)
        _ -> mainloop (if p == 1 then 2 else 1) state'

handleChaos state = do
  n <- randomIO :: IO Int
  if n < 0 then handleChaos state else do
    let remaining' = Set.toList (remaining state)
    let ind = n `mod` length (remaining')
    let (_, state') = updateState (NewCard (number (remaining' !! ind))) state
    return state'
