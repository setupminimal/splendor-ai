module Eval where

import Control.Parallel

pmap f [] = []
pmap f (x:xs) = let fx = f x in fx `par` (fx : pmap f xs)
