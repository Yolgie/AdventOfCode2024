import Data.List.Split (splitOn)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let (rulesString, printRunsString) = splitAtEmptyLine contents
        rules = map parseRule (lines rulesString)
        printRuns = map parsePrintRun (lines printRunsString)
        validPrintRuns = filter (`validatePrintRun` rules) printRuns
        middles = map getMiddleNumber validPrintRuns
        result = sum middles

    print result

splitAtEmptyLine :: String -> (String, String)
splitAtEmptyLine input =
  let (before, after) = span (/= "") (lines input)
  in (unlines before, unlines (drop 1 after))

parseRule :: String -> (Int, Int)
parseRule input =
  case splitOn "|" input of
    [a, b] -> (read a, read b)
    _      -> error "Invalid input"

parsePrintRun :: String -> [Int]
parsePrintRun input = map read (splitOn "," input)

validatePrintRun :: [Int] -> [(Int, Int)] -> Bool
validatePrintRun [] _ = True
validatePrintRun _ [] = True
validatePrintRun (current:printRun) rules | any (\(a, b) -> b == current && elem a printRun) rules = False
                                          | otherwise = validatePrintRun printRun futureRelevantRules
  where futureRelevantRules = filter (\(a, b) -> elem a printRun && elem b printRun) rules

getMiddleNumber :: [Int] -> Int
getMiddleNumber list = list !! (length list `div` 2)