module Defs where

import Data.Char
import Data.List

data Pair = I Int
          | SB Pair Pair   -- <Int, Int>
          | DB Pair Pair   -- <<Int, Int>>

instance Show Pair where 
  show (SB a b) = "<" ++ (show a) ++ "," ++ (show b) ++ ">"
  show (DB a b) = "<<" ++ (show a) ++ "," ++ (show b) ++ ">>"

data Label = L Int

instance Show Label where
  show (L a) = "L" ++ (show a)

data Instr = HALT | RPLUS Int Label | RMINUS Int Label Label

instance Show Instr where
  show HALT = "HALT"
  show (RPLUS a l) = "R" ++ (show a) ++ "+, " ++ (show l)
  show (RMINUS a l l') = "R" ++ (show a) ++ "-, " ++ (show l) ++ ", " ++ (show l')

type Line = (Label, Instr)

--instance of Show Line where
--  show (l, i) = (show l) ++ ": " ++ (show i) 

newtype Binary = Bin {toString :: String}

instance Show Binary where
  show (Bin s) = "0b" ++ s

--data Addr = Ad Int

newtype Addr = Ad {toInt :: Int}

instance Show Addr where 
  show (Ad a) = (show a)

type List = [Addr]

type Program = [Line]

-- <<x, y>> = 2^x(2y+1)
-- <x, y>   = 2^x(2y+1) - 1

decToRevBin :: Int -> Binary
decToRevBin x = Bin (decToRevBin' x)

decToRevBin' :: Int -> String
decToRevBin' 0 = []
decToRevBin' y = (intToDigit b):(decToRevBin' a)
  where 
    (a,b) = quotRem y 2

binToDec :: String -> Int
binToDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0


-- for Q1b
p = [(L 0, RMINUS 1 (L 2) (L 1)),
     (L 1, HALT),
     (L 2, RMINUS 1 (L 3) (L 4)),
     (L 3, RMINUS 1 (L 5) (L 4)),
     (L 4, HALT),
     (L 5, RPLUS 0 (L 0))
    ]

