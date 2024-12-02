import Data.List (sort)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let reportStrings = lines contents
        reports = readReport reportStrings
        isSafe = map checkIfSafe reports
        result = length (filter id isSafe)

    print result

readReport :: [String] -> [[Int]]
readReport = map (map read . words)

checkIfSafe :: [Int] -> Bool
checkIfSafe report = checkIncreasing report [] False || checkDecreasing report [] False
    where
        checkIncreasing :: [Int] -> [Int] -> Bool -> Bool
        checkIncreasing [_] _ _ = True -- too little remaining to compare
        checkIncreasing (prev:current:remainder) head removed
            | prev < current && current - prev <= 3 = checkIncreasing (current:remainder) (head ++ [prev]) removed
            | not removed = -- not used the buffer yet
                checkIncreasing (head ++ [prev] ++ remainder) [] True || checkIncreasing (head ++ [current] ++ remainder) [] True
            | otherwise = False

        checkDecreasing :: [Int] -> [Int] -> Bool -> Bool
        checkDecreasing [_] _ _ = True
        checkDecreasing (prev:current:remainder) head removed
            | prev > current && prev - current <= 3 = checkDecreasing (current:remainder) (head ++ [prev]) removed
            | not removed =
                checkDecreasing (head ++ [prev] ++ remainder) [] True || checkDecreasing (head ++ [current] ++ remainder) [] True
            | otherwise = False