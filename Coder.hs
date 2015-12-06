module Coder where

import Defs

toSinglePair :: Int -> Pair
toSinglePair x = toSinglePair' b
  where
    (Bin b) = decToRevBin x

toSinglePair' :: String -> Pair
toSinglePair' str = DB x y
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
toDoublePair' str = DB x y
  where
    strlen = (length str)
    xstrip = dropWhile ((==) '0') str
    x      = strlen - (length xstrip)
    ybinr  = drop 1 xstrip
    y      = binToDec (reverse ybinr)


pairToInt :: Pair -> Int
pairToInt (DB x y) = (2^x)*(2*y+1)
pairToInt (SB x y) = (2^x)*(2*y+1) - 1


listToInt :: List -> Int
listToInt Empty = 0
listToInt (Li x (Ad l)) = (2^x)*(2*y+1)
  where
    y = listToInt l


intToList :: Int -> List
intToList i = intToList' x
  where
    Bin x = decToRevBin i


intToList' :: String -> List
intToList' str = Li x (Ad l)
  where
    x = length (takeWhile ((==) '0') str)
    l = intToList' (drop 1 (dropWhile ((==) '0') str))




