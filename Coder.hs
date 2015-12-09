module Coder where

import Defs

toSinglePair :: Int -> Pair
toSinglePair = toSinglePair' . toString . decToRevBin

toSinglePair' :: String -> Pair
toSinglePair' str = SB (I x) (I y)
  where
    strlen = (length str)
    xstrip = dropWhile ((==) '1') str
    x      = strlen - (length xstrip)
    y      = binToDec . reverse . tail $ xstrip


toDoublePair :: Int -> Pair
toDoublePair = toDoublePair' . toString . decToRevBin

toDoublePair' :: String -> Pair
toDoublePair' str = DB (I x) (I y)
  where
    strlen = (length str)
    xstrip = dropWhile ((==) '0') str
    x      = strlen - (length xstrip)
    y      = binToDec . reverse . tail $ xstrip


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
progToInt' [] = []
progToInt' ((l, HALT):ls)          = (Ad 0):(progToInt' ls)
progToInt' ((l, RPLUS i (L j)):ls) = (Ad body):(progToInt' ls)
  where
    body = pairToInt (DB (I (2*i)) (I j))
progToInt' ((l, RMINUS i (L j) (L k)):ls) = (Ad body):(progToInt' ls)
  where
    body = pairToInt (DB (I (2*i+1)) (SB (I j) (I k)))

-- L0: R0 L0 L2
-- L1: HALT

intToProg :: Int -> Program
intToProg i = zip [L n | n <- [0..leng]] (map (intToInstr . toInt) xs)
  where
    xs   = intToList i
    leng = length xs



intToInstr :: Int -> Instr
intToInstr 0 = HALT
intToInstr x
  |y `mod` 2 == 0 = RPLUS i (L z)
  |otherwise      = RMINUS i (L j) (L k)
    where
      DB a b = (toDoublePair x)
      y      = pairToInt a
      z      = pairToInt b 
      i      = y `div` 2
      SB c d = (toSinglePair z)
      j      = pairToInt c
      k      = pairToInt d 













