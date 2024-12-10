import Data.Char (isDigit, digitToInt)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let input = convertToIdAtPositionList . addIndices . parseSingleDigits $ contents
      targetLength = length (filter (\x -> x >= 0) input)
      defragmented = defragment input targetLength
      multiplied = zipWith (\a b -> fromIntegral a * fromIntegral b) ([0..] :: [Int]) defragmented
      result = sum multiplied

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
defragment :: [Int] -> Int -> [Int]
defragment input targetLenght = defragment' [] input (reverse input) targetLenght

defragment' :: [Int] -> [Int] -> [Int] -> Int -> [Int]
defragment' acc _ _ 0 = reverse acc
defragment' acc input (y:ys) currentCount  | y == -1 = defragment' acc input ys currentCount
defragment' acc (x:xs) (y:ys) currentCount | x /= -1 = defragment' (x:acc) xs (y:ys) (currentCount -1)
                                           | x == -1 = defragment' (y:acc) xs ys (currentCount -1)
defragment' acc _ _ _ = reverse acc