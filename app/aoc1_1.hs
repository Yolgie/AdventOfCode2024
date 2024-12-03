import Data.List (sort)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let linesOfFile = lines contents
        inputPairs = map parseLineToIntTuple linesOfFile
        (inputFirsts, inputSeconds) = unzip inputPairs
        sortedFirsts = sort inputFirsts
        sortedSeconds = sort inputSeconds
        sortedPairs = zip sortedFirsts sortedSeconds
        distances = map (\(a,b) -> abs (a - b)) sortedPairs
        sumDistances = sum distances

    print sortedPairs
    print distances
    print sumDistances

parseLineToIntTuple :: String -> (Int, Int)
parseLineToIntTuple line =
    let [a,b] = words line
    in (read a, read b)
