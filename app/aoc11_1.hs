
main :: IO ()
main = do
    contents <- readFile "input.txt"
    let input = map read . words $ contents
        result = applyNTimes 25 getNextState $ input

--    print result
    print (length result)

applyNTimes :: Int -> ([a] -> [a]) -> [a] -> [a]
applyNTimes n f xs = iterate f xs !! n

getNextState :: [Integer] -> [Integer]
getNextState = concatMap getNextState'

getNextState' :: Integer -> [Integer]
getNextState' 0 = [1]
getNextState' x | even (getNumberOfDigits x) = splitInHalf x
                | otherwise = [x * 2024]

getNumberOfDigits :: Integer -> Int
getNumberOfDigits = length . show

splitInHalf :: Integer -> [Integer]
splitInHalf int =
  let intString = show int
      midpoint = length intString `div` 2
      (firstHalf, secondHalf) = splitAt midpoint intString
  in [read firstHalf, read secondHalf]
