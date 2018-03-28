module Main where

import Search
import Splendor
import Data
import Eval

depth :: Int
depth = 4 -- in turns, not plys

main :: IO ()
main = mainloop initialState

mainloop :: State -> IO ()
mainloop state = do
  putStrLn "Current State:"
  print state
  putStrLn "Calculating ..."
  let move = bestFrom state :: Edit
  let (_, state') = updateState move state
  print state'
  putStrLn $ "Do: " ++ show move
  x <- readLn :: IO Edit
  case x of
    Quit -> return ()
    other -> let (_, state'') = updateState other state' in mainloop state''

minBy :: Ord a => (b -> a) -> [b] -> b
minBy f l = snd $ go $ zip (pmap f l) l
  where
    go [] = error "Minimum of empty list"
    go [x] = x
    go ((c, x):xs) = let (recc, recx) = go xs in if c < recc then (c, x) else (recc, recx)

bestFrom :: State -> Edit
bestFrom state = minBy (\m -> dispatch (updateState m state) depth minBound maxBound) moves
  where
    moves = children state
