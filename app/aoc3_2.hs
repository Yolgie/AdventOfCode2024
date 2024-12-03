import Text.Regex.TDFA
import Data.List

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let input = contents
        numbers = parseInputToInts input True []
--        dontRanges = parseDontFlagsFrom input
        multiplied = map product numbers
        total = sum multiplied

    print numbers
    print total

--parseDontFlagsFrom :: String -> [(Int, Int)]
--parseDontFlagsFrom input =
--    let dontPattern = "don't\\(\\)"
--        dontMatches = input =~ dontPattern :: AllMatches [] (MatchOffset, MatchLength)
--        doPattern = "do\\(\\)"
--        doMatches = input =~ doPattern :: AllMatches [] (MatchOffset, MatchLength)
--        dontOffsets = map fst (getAllMatches dontMatches)
--        doOffsets = map fst (getAllMatches doMatches) ++ [length input]
--        dontRange = map (\start -> (start, nextHighest start)) dontOffsets
--        nextHighest :: Int -> Int
--        nextHighest lowerBound = minimum (filter (> lowerBound) doOffsets)
--    in dontRange

pattern :: String
pattern = "^mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"

parseInputToInts :: String -> Bool -> [[Int]] -> [[Int]]
parseInputToInts [] _ result = result
parseInputToInts input _ result | Just remainder <- stripPrefix "do()" input = parseInputToInts remainder True result
parseInputToInts input _ result | Just remainder <- stripPrefix "don't()" input = parseInputToInts remainder False result
parseInputToInts (_:remainder) False result = parseInputToInts remainder False result
parseInputToInts input True result | input =~ pattern :: Bool = let (_, _, remainder, numbers) = input =~ pattern ::  (String, String, String, [String])
                                                                 in parseInputToInts remainder True (result ++ [[read (numbers !! 0), read (numbers !! 1)]])
                                   | otherwise = parseInputToInts (tail input) True result
--parseInputToInts (_:remainder) flag result = parseInputToInts remainder flag result


--prefixInt :: String -> Maybe (Int, String)
--
--
--parseInputToInts (stripPrefix "do()" -> Just remainder) _ result = parseInputToInts remainder True result
--parseInputToInts ["don't()":remainder] _ result = parseInputToInts remainder False result
--parseInputToInts ["mul(":remainder] False result = parseInputToInts remainder False result
--parseInputToInts ["mul(":x1:",":y1:")":remainder] True result             | all (isDigit) [x1,y1]             = parseInputToInts remainder True (result ++ [[read x1, read y1]])
--parseInputToInts ["mul(":x1:x2:",":y1:")":remainder] True result          | all (isDigit) [x1,x2,y1]          = parseInputToInts remainder True (result ++ [[read x1 ++ x2, read y1]])
--parseInputToInts ["mul(":x1:x2:x3:",":y1:")":remainder] True result       | all (isDigit) [x1,x2,x3,y1]       = parseInputToInts remainder True (result ++ [[read x1 ++ x2 ++ x3, read y1]])
--parseInputToInts ["mul(":x1:",":y1:y2:")":remainder] True result          | all (isDigit) [x1,y1,y2]          = parseInputToInts remainder True (result ++ [[read x1, read y1 ++ y2]])
--parseInputToInts ["mul(":x1:x2:",":y1:y2:")":remainder] True result       | all (isDigit) [x1,x2,y1,y2]       = parseInputToInts remainder True (result ++ [[read x1 ++ x2, read y1 ++ y2]])
--parseInputToInts ["mul(":x1:x2:x3:",":y1:y2:")":remainder] True result    | all (isDigit) [x1,x2,x3,y1,y2]    = parseInputToInts remainder True (result ++ [[read x1 ++ x2 ++ x3, read y1 ++ y2]])
--parseInputToInts ["mul(":x1:",":y1:y2:y3:")":remainder] True result       | all (isDigit) [x1,y1,y2,y3]       = parseInputToInts remainder True (result ++ [[read x1, read y1 ++ y2 ++ y3]])
--parseInputToInts ["mul(":x1:x2:",":y1:y2:y3:")":remainder] True result    | all (isDigit) [x1,x2,y1,y2,y3]    = parseInputToInts remainder True (result ++ [[read x1 ++ x2, read y1 ++ y2 ++ y3]])
--parseInputToInts ["mul(":x1:x2:x3:",":y1:y2:y3:")":remainder] True result | all (isDigit) [x1,x2,x3,y1,y2,y3] = parseInputToInts remainder True (result ++ [[read x1 ++ x2 ++ x3, read y1 ++ y2 ++ y3]])
--parseInputToInts (_:remainder) flag result = parseInputToInts remainder flag result
--
--    let pattern = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"
--        doPattern = "do\\(\\)"
--        dontPattern = "don't\\(\\)"
--        matchPattern :: String -> [[Int]] -> Bool -> [[Int]]
--        matchPattern "" result _ = result
--        matchPattern input ints False =
--            let (before, _, after, matches) = input =~ pattern :: (String, String, String, [String])
--                newInts = map read matches
--                newFlag = befor =~ doPattern ::
--            in matchPattern after (ints ++ [newInts]) False
--
--            in
--    in map extractMatch matches
--    where
--        extractMatch :: (_, _, _, [String]) -> (Int, Int)
--        extractMatch foundMatch =
--            let  = foundMatch !! 0    -- Starting index of the match
--                (_, _, a) = foundMatch !! 1         -- First captured group
--                (_, _, b) = foundMatch !! 2         -- Second captured group
--            in (offset, read a, read b)