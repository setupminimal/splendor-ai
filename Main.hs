module Main where

import Search
import Splendor
import Data

depth = 4 -- in turns, not plys

main = do
  mainloop initialState

mainloop state = do
  putStrLn "Current State:"
  print $ state
  putStrLn "Calculating ..."
  let (state', move) = bestFrom state :: (State, Edit)
  print $ state'
  putStrLn $ "Do: " ++ show move
  x <- readLn :: IO Edit
  case x of
    Quit -> return ()
    x -> let (_, state'') = updateState x state' in mainloop state''

bestFrom state = best minBound undefined undefined moves
  where
    moves = children state
    best bScore mv res [] = (res, mv)
    best bScore mv res (m:ms) | score > bScore = best score m res' ms
      where
        (next, res') = updateState m state
        score = dispatch (next, res') depth minBound maxBound
    best bScore mv res (m:ms) = best bScore mv res ms
