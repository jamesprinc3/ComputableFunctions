module Defs where

import Data.Char
import Data.List

data Pair = SB Int Int   -- <Int, Int>
          | DB Int Int   -- <<Int, Int>>

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

data Line = X Label Instr

instance Show Line where
  show (X l i) = (show l) ++ ": " ++ (show i) 

data Binary = Bin String

instance Show Binary where
  show (Bin s) = "0b" ++ s


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