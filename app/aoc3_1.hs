import Text.Regex.TDFA

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let input = contents
        numbers = parseInputToIntTuples input
        multiplied = map (\(x,y) -> x * y) numbers
        total = sum multiplied

    print total

parseInputToIntTuples :: String -> [(Int, Int)]
parseInputToIntTuples input =
    let pattern = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"
        matches = input =~ pattern :: [[String]]
    in map (\[_, a, b] -> (read a, read b)) matches
