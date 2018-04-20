module Eval where

import Control.Parallel

pmap f [] = []
pmap f (x:xs) = f x `par` pmap f xs `par` (f x : pmap f xs)
