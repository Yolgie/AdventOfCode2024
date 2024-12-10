import Data.Char (isDigit, digitToInt)

main :: IO ()
main = do
  contents <- readFile "test.txt"
  let input = convertToIdAtPositionList . addIndices . parseSingleDigits $ contents
      defragmented = defragment input
      multiplied = zipWith (\a b -> fromIntegral a * fromIntegral b) ([0..] :: [Int]) defragmented
      result = sum multiplied

  print input
  print defragmented
  print result

parseSingleDigits :: String -> [Int]
parseSingleDigits = map digitToInt . filter isDigit

addIndices :: [Int] -> [(Int, Int)]
addIndices xs = zip indices xs
  where
    indices = concatMap (\x -> [x, -1]) [0 ..]

-- gets a list of tuples where the first is the id and the second the size and returns a list of ids (duplicated for each size)
convertToIdAtPositionList :: [(Int, Int)] -> [Int]
convertToIdAtPositionList = concatMap (\(blockId, size) -> replicate size blockId)

-- defragment by always taking the last block to fill empty space (denoted by -1)
defragment :: [Int] -> [Int]
defragment = defragment' []

defragment' :: [Int] -> [Int] -> [Int]
defragment' acc [] = acc
defragment' acc input  | last input == -1 = defragment' acc (init input)
defragment' acc (x:xs) | x == -1 = defragment' (acc ++ [last xs]) (init xs)
                       | otherwise = defragment' (acc ++ [x]) xs