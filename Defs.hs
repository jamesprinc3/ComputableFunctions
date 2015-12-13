module Defs where

import Data.Char
import Data.List

data Pair = I Integer
          | SB Pair Pair   -- <Int, Integer>
          | DB Pair Pair   -- <<Int, Integer>>

instance Show Pair where 
  show (SB a b) = "<" ++ (show a) ++ "," ++ (show b) ++ ">"
  show (DB a b) = "<<" ++ (show a) ++ "," ++ (show b) ++ ">>"

data Label = L Integer

instance Show Label where
  show (L a) = "L" ++ (show a)

data Instr = HALT | RPLUS Integer Label | RMINUS Integer Label Label

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

--data Addr = Ad Integer

newtype Addr = Ad {toInt :: Integer}

instance Show Addr where 
  show (Ad a) = (show a)

type List = [Addr]

type Program = [Line]

-- <<x, y>> = 2^x(2y+1)
-- <x, y>   = 2^x(2y+1) - 1

decToRevBin :: Integer -> Binary
decToRevBin x = Bin (decToRevBin' x)

decToRevBin' :: Integer -> String
decToRevBin' 0 = []
decToRevBin' y = ((intToDigit . fromInteger) b):(decToRevBin' a)
  where 
    (a,b) = quotRem y 2

binToDec :: String -> Integer
binToDec = foldl' (\acc x -> acc * 2 + (toInteger . digitToInt) x) 0


-- for Q1c
p = [(L 0, RMINUS 1 (L 2) (L 1)),
     (L 1, HALT),
     (L 2, RMINUS 1 (L 3) (L 4)),
     (L 3, RMINUS 1 (L 5) (L 4)),
     (L 4, HALT),
     (L 5, RPLUS 0 (L 0))
    ]
-- Answer to Q1c given by: map (instrToInt . snd)  p
-- [46,0,286,1150,0,1]

-- for Q2
n :: Integer
n = (2^46) * 20483
-- Answer for Q2 given by intToProg n 
-- [(L0,R0-, L2, L1),(L1,HALT),(L2,R0-, L0, L1),(L3,R0+, L0)]

