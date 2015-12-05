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
