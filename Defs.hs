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

data Line = Label Instr
