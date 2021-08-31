module Util (map2d) where

import Debug.Trace

map2d :: (a -> b) -> [[a]] -> [[b]]
map2d = map . map

dbg :: (Show a) => a -> a
dbg x = trace (show x) x
