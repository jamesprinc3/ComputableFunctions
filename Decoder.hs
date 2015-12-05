module Decoder where

import Defs



pairToInt :: Pair -> Int
pairToInt (DB x y) = (2^x)*(2*y+1)
pairToInt (SB x y) = (2^x)*(2*y+1) - 1

