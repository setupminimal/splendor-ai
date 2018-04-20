{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Concurrent.ParallelIO
import Control.Exception

import Data.List
import qualified Data.Set as Set

import System.IO
import System.Random

import Search
import Splendor
import Data

depth = 4 -- fast turns

--algo1 = ("Basic", evalBasic)
--algo2 = ("Relative", evalRelative)

algosToTry = [("Basic", evalBasic), ("Relative", evalRelative), ("Card Buy", evalCardBuy), ("Joshua", evalJoshua)]

trials = 500

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  let trials = (uncurry main') (("Card Buy", evalCardBuy), ("Joshua", evalJoshua))
  trials
  stopGlobalPool

mbShH = do
  c <- randomRIO (0, 10) :: IO Int
  if c == 0 then putStr "#" >> return (Just "chickens") else return (Just "chickens")

main' algo1 algo2 = do
  --hSetBuffering stdout NoBuffering
  n <- parallel . intersperse mbShH . map (\r -> unexceptional (mainloop r 1 initialState)) $ (replicate trials [undefined, algo1, algo2]) ++ (replicate trials [undefined, algo2, algo1])
  let nExp = length . filter (== Nothing) $ n
  putStrLn $ "\nAlgorithm " ++ fst algo1 ++ " won to " ++ fst algo2 ++ " in " ++ show (length . filter (== (Just $ fst algo1)) $ n) ++ " out of " ++ show (2 * trials - nExp) ++ " matches."
  if nExp /= 0 then putStrLn $ "There were " ++ show nExp ++ " exceptions that ended matches early." else return ()

unexceptional a = catch @SomeException
                  (a)
                  (\x -> print x >> return Nothing)

mainloop :: ([(String, Int -> State -> Int)]) -> Int -> State -> IO (Maybe String)
mainloop algos p state = do
  if null $ children state then print state else return ()
  let (escore, ed) = maxNode (snd $ algos !! p) p state depth minBound maxBound
  if ed == Nothing || ed == Just Win then {- (putStr $ if p == 1 then "#" else ".") >> -} return (Just . fst $ algos !! p)
    else
    let (next, state') = updateState (fromJust ed) state in
      case next of
        Chaos -> handleChaos state' >>= mainloop algos (if p == 1 then 2 else 1)
        _ -> mainloop algos (if p == 1 then 2 else 1) state'

handleChaos state = do
  let nRemain = length . Set.toList $ remaining state
  n <- randomRIO (0, nRemain - 1) :: IO Int
  let tk = if nRemain == 0 then NoNewCard else (NewCard (number (Set.toList (remaining state) !! n)))
  let (_, state') = updateState tk state
  return state'
