import Data.List (sort)

main :: IO ()
main = do
    contents <- readFile "Input.txt"
    let reportStrings = lines contents
        reports = readReport reportStrings
        isSafe = map checkIfSafe reports
        result = length (filter id isSafe)

    print reports
    print isSafe
    print result

readReport :: [String] -> [[Int]]
readReport = map (map read . words)

checkIfSafe :: [Int] -> Bool
checkIfSafe report =
    let allIncreasing = all (\(x1, x2) -> x2 > x1 && x2 - x1 <=3) (zip report (tail report))
        allDecreasing = all (\(x1, x2) -> x2 < x1 && x1 - x2 <=3) (zip report (tail report))
    in allIncreasing || allDecreasing

