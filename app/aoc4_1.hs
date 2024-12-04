import Data.List

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let input = lines contents
        input45 = transformInputToDiagonal input
        input90 = transpose input
        input135 = transformInputToDiagonal (map reverse input90)
        allInputs = concat [input, input45, input90, input135]
        count = sum (map (countSubstring "XMAS") allInputs)
        countReverse = sum (map (countSubstring "SAMX") allInputs)

--    print allInputs
    print (count + countReverse)

countSubstring :: String  -- Substring to search for
               -> String  -- Main string
               -> Int     -- > Number of occurrences
countSubstring _ [] = 0
countSubstring substring input
    | length input < length substring               = 0
    | Just remainder <- stripPrefix substring input = 1 + countSubstring substring remainder
    | otherwise                                     = countSubstring substring (tail input)

-- recursive function to rotate the input 45 degrees clockwise
transformInputToDiagonal :: [String] -> [String]
transformInputToDiagonal input = transformInputToDiagonal' [] input []

-- head -> tail -> acc -> result :: go through all lines and start the collecting of the new lines
transformInputToDiagonal' :: [String] -> [String] -> [String] -> [String]
transformInputToDiagonal' _ [] result = result
transformInputToDiagonal' _ [""] result = result
transformInputToDiagonal' inputHead [x] acc = let newInputHead = inputHead ++ [x]
                                                  trimmedHead = map tail inputHead
                                              in transformInputToDiagonal' trimmedHead [tail x] (acc ++ [(transformInputToDiagonal'' (reverse newInputHead) [])])
transformInputToDiagonal' inputHead (x:xs) acc = let newInputHead = inputHead ++ [x]
                                                 in transformInputToDiagonal' newInputHead xs (acc ++ [(transformInputToDiagonal'' (reverse newInputHead) [])])

-- input -> acc -> result :: always take the first char and then continue with the 2. char of the next line
transformInputToDiagonal'' :: [String] -> String -> String
transformInputToDiagonal'' [] result = result
transformInputToDiagonal'' emptyLists result | all null emptyLists = result
transformInputToDiagonal'' (x:xs) acc = transformInputToDiagonal'' (map tail (filter (not . null) xs)) (acc ++ [head x])

-- orientations needed 0, 45, 90, 135 if checking XMAS and SAMX