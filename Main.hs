{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception

import System.Environment
import System.IO

import Search
import Splendor
import Data

depth :: Int
depth = 7 -- in turns, not plys

main :: IO ()
main = do
  args <- getArgs
  let quiet = if "quiet" `elem` args then (\_ -> return ()) else id
  if "second" `elem` args
    then do
      firstMove <- readLn :: IO Edit
      mainloop 2 (snd $ updateState firstMove initialState) quiet
    else
      mainloop 1 initialState quiet

mainloop :: Int -> State -> (IO () -> IO ()) -> IO ()
mainloop player' state quiet | player state /= player' = do
                                 quiet $ print state
                                 quiet $ putStrLn "... and then?"
                                 x <- catch @SomeException
                                      (readLn :: IO Edit)
                                      (\_ -> putStrLn "Try again." >> mainloop player' state quiet >> return Quit)
                                 mainloop player' (snd $ updateState x state) quiet
mainloop player state quiet = do
  quiet $ putStrLn "Current State:"
  quiet $ print state
  quiet $ putStrLn "Calculating ..."
  let (score, edit) = bestFrom state player
  quiet $ putStrLn $ "Calculated Score: " ++ show score
  lhf player state quiet edit

lhf player state quiet Nothing = do
  putStrLn "I don't know what to do, something is wrong."
  case children state of
    [] -> putStrLn "There is no move to make."
    (x:xs) -> do
      let move = head (children state)
      putStrLn $ "Trying: " ++ show move
      lhf player state quiet (Just move)
lhf player state quiet (Just move) = do
  let (_, state') = updateState move state
  quiet $ print state'
  quiet $ putStrLn "Do: "
  print move
  x <- readMove
  case x of
    Quit -> return ()
    other -> let (_, state'') = updateState other state' in mainloop player state'' quiet

bestFrom :: State -> Int -> (Int, Maybe Edit)
bestFrom state player = maxNode Min evalJoshua player state depth minBound maxBound

readMove = do
  x <- catch @SomeException
       (readLn :: IO Edit)
       (\_ -> putStrLn "\n\nWhat?\n\n" >> readMove)
  return x
