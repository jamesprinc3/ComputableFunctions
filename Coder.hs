module Coder where

import Defs

toSinglePair :: Int -> Pair
toSinglePair x = toSinglePair' b
  where
    (Bin b) = decToRevBin x

toSinglePair' :: String -> Pair
toSinglePair' str = SB (I x) (I y)
  where
    strlen = (length str)
    xstrip = dropWhile ((==) '1') str
    x      = strlen - (length xstrip)
    ybinr  = drop 1 xstrip
    y      = binToDec (reverse ybinr)


toDoublePair :: Int -> Pair
toDoublePair x = toDoublePair' b
  where
    (Bin b) = decToRevBin x

toDoublePair' :: String -> Pair
toDoublePair' str = DB (I x) (I y)
  where
    strlen = (length str)
    xstrip = dropWhile ((==) '0') str
    x      = strlen - (length xstrip)
    ybinr  = drop 1 xstrip
    y      = binToDec (reverse ybinr)


pairToInt :: Pair -> Int
pairToInt (I i)    = i
pairToInt (DB x y) = (2^(pairToInt x))*(2*(pairToInt y)+1)
pairToInt (SB x y) = (2^(pairToInt x))*(2*(pairToInt y)+1) - 1


listToInt :: List -> Int
listToInt [] = 0
listToInt ((Ad x):xs) = (2^x)*(2*y+1)
  where
    y = listToInt xs


intToList :: Int -> List
intToList i = intToList' x
  where
    Bin x = decToRevBin i


intToList' :: String -> List
intToList' str = (Ad x):xs
  where
    x = length (takeWhile ((==) '0') str)
    xs = intToList' (drop 1 (dropWhile ((==) '0') str))

-- data Instr = HALT | RPLUS Int Label | RMINUS Int Label Label (for ref)

progToInt :: Program -> Int
progToInt ps = (listToInt (progToInt' ps))

progToInt' :: Program -> List
progToInt' ((l, HALT):ls)          = (Ad 0):(progToInt' ls)
progToInt' ((l, RPLUS i (L j)):ls) = (Ad body):(progToInt' ls)
  where
    body = pairToInt (DB (I (2*i)) (I j))
progToInt' ((l, RMINUS i (L j) (L k)):ls) = (Ad body):(progToInt' ls)
  where
    body = pairToInt (DB (I (2*i+1)) (SB (I j) (I k)))




