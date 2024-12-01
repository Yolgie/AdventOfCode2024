import Data.List (sort)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let linesOfFile = lines contents
        inputPairs = map parseLineToIntTuple linesOfFile
        (inputFirsts, inputSeconds) = unzip inputPairs
        similarScores = map (\number -> number * (countOccurence number)) inputFirsts
        sumSimilars = sum similarScores
        countOccurence :: Int -> Int
        countOccurence number = length (filter (==number) inputSeconds)

    print similarScores
    print sumSimilars

parseLineToIntTuple :: String -> (Int, Int)
parseLineToIntTuple line =
    let [a,b] = words line
    in (read a, read b)
