module Eval where

import Control.Parallel

pmap :: (a -> b) -> [a] -> [b]
pmap _ [] = []
pmap f (x:xs) = let fx = f x in fx `par` (fx : pmap f xs)
