{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception

import System.IO

import Search
import Splendor
import Data
import Eval

depth :: Int
depth = 6 -- in turns, not plys

main :: IO ()
main = do
  hndl <- openFile "game.splndr" WriteMode
  mainloop initialState hndl

mainloop :: State -> Handle -> IO ()
mainloop state handle = do
  putStrLn "Current State:"
  print state
  putStrLn "Calculating ..."
  let (score, edit) = bestFrom state
  putStrLn $ "Calculated Score: " ++ show score
  case edit of
    Nothing -> putStrLn "I don't know what to do, please advise."
    Just move -> do
      let (_, state') = updateState move state
      print state'
      putStrLn $ "Do: " ++ show move
      x <- catch @SomeException
           (readLn :: IO Edit)
           (\_ -> putStrLn "What?" >> mainloop state handle >> return Quit)
      hPutStrLn handle (show x)
      case x of
        Quit -> return ()
        other -> let (_, state'') = updateState other state' in mainloop state'' handle

bestFrom :: State -> (Int, Maybe Edit)
bestFrom state = maxNode state depth minBound maxBound
