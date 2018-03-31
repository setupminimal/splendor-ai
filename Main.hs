{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception

import System.Environment
import System.IO

import Search
import Splendor
import Data

depth :: Int
depth = 8 -- in turns, not plys

main :: IO ()
main = do
  args <- getArgs
  hndl <- openFile (head args) WriteMode
  let quiet = if "quiet" `elem` args then (\_ -> return ()) else id
  if "second" `elem` args
    then do
      firstMove <- readLn :: IO Edit
      mainloop 2 (snd $ updateState firstMove initialState) hndl quiet
    else
      mainloop 1 initialState hndl quiet

mainloop :: Int -> State -> Handle -> (IO () -> IO ()) -> IO ()
mainloop player' state handle quiet | player state /= player' = do
                                       quiet $ print state
                                       quiet $ putStrLn "... and then?"
                                       x <- catch @SomeException
                                            (readLn :: IO Edit)
                                            (\_ -> putStrLn "Try again." >> mainloop player' state handle quiet >> return Quit)
                                       mainloop player' (snd $ updateState x state) handle quiet
mainloop player state handle quiet = do
  quiet $ putStrLn "Current State:"
  quiet $ print state
  quiet $ putStrLn "Calculating ..."
  let (score, edit) = bestFrom state player
  quiet $ putStrLn $ "Calculated Score: " ++ show score
  lhf player state handle quiet edit

lhf player state handle quiet Nothing = do
  putStrLn "I don't know what to do, something is wrong."
  let move = head (children state)
  putStrLn $ "Trying: " ++ show move
  lhf player state handle quiet (Just move)
lhf player state handle quiet (Just move) = do
  let (_, state') = updateState move state
  quiet $ print state'
  quiet $ putStrLn "Do: "
  putStrLn $ show move
  x <- catch @SomeException
       (readLn :: IO Edit)
       (\_ -> putStrLn "What?" >> mainloop player state handle quiet >> return Quit)
  hPutStrLn handle (show x)
  hFlush handle
  case x of
    Quit -> hClose handle >> return ()
    other -> let (_, state'') = updateState other state' in mainloop player state'' handle quiet

bestFrom :: State -> Int -> (Int, Maybe Edit)
bestFrom state player = maxNode player state depth minBound maxBound
