{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Concurrent.ParallelIO
import Control.Exception

import qualified Data.Set as Set

import System.IO
import System.Random

import Search
import Splendor
import Data

depth = 4 -- fast turns

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  n <- parallel . map (\i -> unexceptional (mainloop i initialState)) $ (replicate 250 1) ++ (replicate 250 2)
  putStrLn $ "\nAlgorithm 1 won " ++ show (length . filter (== 1) $ n) ++ " out of 500 matches."
  stopGlobalPool

unexceptional a = catch @SomeException
                  (a)
                  (\_ -> return (-1))

mainloop :: Int -> State -> IO Int
mainloop p state = do
  if null $ children state then print state else return ()
  let (escore, ed) = maxNode p state depth minBound maxBound
  if ed == Nothing || ed == Just Win then (putStr $ if p == 1 then "#" else ".") >> return p
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
    let tk = if null remaining' then NoNewCard else (NewCard (number (remaining' !! ind)))
    let (_, state') = updateState tk state
    return state'
