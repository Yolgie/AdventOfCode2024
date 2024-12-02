import Data.List (sort)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let reportStrings = lines contents
        reports = readReport reportStrings
        isSafe = map checkIfSafe reports
        result = length (filter id isSafe)
        reportsWithResults = zip3 reports isSafe (map checkIfSafeOld reports)

    mapM_ print reportsWithResults
    print result

readReport :: [String] -> [[Int]]
readReport = map (map read . words)

checkIfSafeOld :: [Int] -> Bool
checkIfSafeOld report =
    all (\(x1, x2) -> x2 > x1 && x2 - x1 <= 3) (zip report (tail report))
    || all (\(x1, x2) -> x2 < x1 && x1 - x2 <= 3) (zip report (tail report))

checkIfSafe :: [Int] -> Bool
checkIfSafe report = checkIncreasing report False || checkDecreasing report False || checkIncreasing (tail report) True || checkDecreasing (tail report) True
  where
    checkIncreasing [_] _ = True -- any single element is good
    checkIncreasing (first:second:remainder) removed
      | second > first && second - first <= 3 = checkIncreasing (second:remainder) removed
      | not removed = -- not used the buffer yet
          checkIncreasing (first:remainder) True
      | otherwise = False

    checkDecreasing [_] _ = True
    checkDecreasing (first:second:remainder) removed
      | second < first && first - second <= 3 = checkDecreasing (second:remainder) removed
      | not removed =
          checkDecreasing (first:remainder) True
      | otherwise = False
