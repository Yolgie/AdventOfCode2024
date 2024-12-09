import Data.List.Split (splitOn)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let (rulesString, printRunsString) = splitAtEmptyLine contents
        rules = map parseRule (lines rulesString)
        printRuns = map parsePrintRun (lines printRunsString)
        inValidPrintRuns = filter (not . (validatePrintRun rules)) printRuns
        validPrintRuns = map (\x -> makeRunValid x rules) inValidPrintRuns
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

validatePrintRun :: [(Int, Int)] -> [Int] -> Bool
validatePrintRun [] _ = True
validatePrintRun _ [] = True
validatePrintRun rules (current:printRun) | any (\(a, b) -> b == current && elem a printRun) rules = False
                                          | otherwise = validatePrintRun futureRelevantRules printRun
  where futureRelevantRules = filter (\(a, b) -> elem a printRun && elem b printRun) rules

getMiddleNumber :: [Int] -> Int
getMiddleNumber list = list !! (length list `div` 2)

filterForRelevantRulesForRun :: [Int] -> [(Int, Int)] -> [(Int, Int)]
filterForRelevantRulesForRun printRun rules = filter (\(a, b) -> elem a printRun && elem b printRun) rules

filterForRelevantRulesForNumber:: Int -> [(Int, Int)] -> [(Int, Int)]
filterForRelevantRulesForNumber number rules = filter (\(a, b) -> a == number || b == number) rules

rulesContainNumber :: Int -> [(Int, Int)] -> Bool
rulesContainNumber number rules = any (\(a, b) -> a == number || b == number) rules

swapInPrintRun :: [Int] -> Int -> Int -> [Int]
swapInPrintRun printRun a b = map (\x -> if x == a then b else if x == b then a else x) printRun

makeRunValid :: [Int] -> [(Int, Int)] -> [Int]
makeRunValid printRun rules = makeRunValid' [] printRun relevantRules
  where relevantRules = filterForRelevantRulesForRun printRun rules

makeRunValid' :: [Int] -> [Int] -> [(Int, Int)] -> [Int]
makeRunValid' sorted [] _ = sorted
makeRunValid' sorted remainder [] = sorted ++ remainder
makeRunValid' sorted (current:remainder) rules =
  case filter (\(a, b) -> b == current && a `elem` remainder) rules of
    ((a, b):_) -> makeRunValid' sorted (swapInPrintRun (current:remainder) a b) rules
    []         -> makeRunValid' (sorted ++ [current]) remainder (filterForRelevantRulesForRun remainder rules)