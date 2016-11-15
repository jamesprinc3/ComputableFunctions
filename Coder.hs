module Coder where

import Defs

-- Converts from integer z = (2^x)*(2y + 1)−1 -> <x,y> (Slide 18/19)
toSinglePair :: Integer -> Pair
toSinglePair = toSinglePair' . toString . decToRevBin

--TODO: change from String type
toSinglePair' :: String -> Pair
toSinglePair' str = SB (I x) (I y)
  where
    strlen = (length str)
    xstrip = dropWhile ((==) '1') str
    x      = toInteger (strlen - (length xstrip))
    y      = toInteger (binToDec . reverse . tail $ xstrip)


-- Converts from integer z = (2^x)*(2y + 1) -> <<x,y>> (Slide 18/19)
toDoublePair :: Integer -> Pair
toDoublePair = toDoublePair' . toString . decToRevBin

--TODO: change from String type
toDoublePair' :: String -> Pair
toDoublePair' str = DB (I x) (I y)
  where
    strlen = (length str)
    xstrip = dropWhile ((==) '0') str
    x      = toInteger (strlen - (length xstrip))
    y      = toInteger (binToDec . reverse . tail $ xstrip)


-- Converts from a Pair = i | <x,y> | <<x,y>> to an Integer (Slide 18/19)
pairToInt :: Pair -> Integer
pairToInt (I i)    = i
pairToInt (DB x y) = (2^(pairToInt x))*(2*(pairToInt y)+1)
pairToInt (SB x y) = (2^(pairToInt x))*(2*(pairToInt y)+1) - 1

-- TODO: rewrite this as a fold
listToInt :: List -> Integer
listToInt [] = 0
listToInt ((Ad x):xs) = (2^x)*(2*y+1)
  where
    y = listToInt xs

-- Converts from an Integer to a list of encoded instructions (Slide 24)
intToList :: Integer -> List
intToList i = intToList' x
  where
    Bin x = decToRevBin i

-- TODO: Remove string type
intToList' :: String -> List
intToList' []  = []
intToList' str = (Ad x):xs
  where
    x  = toInteger (length (takeWhile ((==) '0') str))
    xs = intToList' (drop 1 (dropWhile ((==) '0') str))

-- Encodes programs as integers (Slide 25)
progToInt :: Program -> Integer
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


-- Decodes ints into programs (Slide 25)
intToProg :: Integer -> Program
-- intToProg i = zip (repeat (L 0)) (map (intToInstr . toInt) xs)
intToProg i = zip [(L (toInteger n)) | n <- [0..leng]] (map (intToInstr . toInt) xs)
  where
    xs   = intToList i
    leng = length xs

-- Decodes ints into their respective instructions (Slide 25)
intToInstr :: Integer -> Instr
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


instrToInt :: Instr -> Integer
instrToInt HALT                   = 0
instrToInt (RPLUS i (L j))        = pairToInt (DB (I (2*i)) (I j))
instrToInt (RMINUS i (L j) (L k)) = pairToInt (DB (I ((2*i)+1)) (SB (I j) (I k)))



