import Data.List

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let input = map read . words $ contents
        compressed = compress input
        result = applyNTimes 75 getNextState $ compressed
        count = sum $ map fst result

    print count

type Counted = [(Int, Integer)]

applyNTimes :: Int -> ([a] -> [a]) -> [a] -> [a]
applyNTimes n f xs = iterate f xs !! n

compress :: [Integer] -> Counted
compress = map (\grp -> (length grp, head grp)) . group . sort

getNextState :: Counted -> Counted
getNextState = mergeCounts . concatMap getNextState'
  where
    getNextState' :: (Int, Integer) -> Counted
    getNextState' (count, 0) = [(count, 1)]
    getNextState' (count, x)
      | even (getNumberOfDigits x) = map (\y -> (count, y)) (splitInHalf x)
      | otherwise = [(count, x * 2024)]

    mergeCounts :: Counted -> Counted
    mergeCounts = map sumGroup . groupBy sameValue . sortOn snd
      where
        sameValue a b = snd a == snd b
        sumGroup grp = (sum (map fst grp), snd (head grp))

getNumberOfDigits :: Integer -> Int
getNumberOfDigits = length . show

splitInHalf :: Integer -> [Integer]
splitInHalf int =
  let intString = show int
      midpoint = length intString `div` 2
      (firstHalf, secondHalf) = splitAt midpoint intString
  in [read firstHalf, read secondHalf]
