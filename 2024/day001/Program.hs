import Data.List (sort, filter)

extractData :: String -> ([Int], [Int])
extractData input = unzip d
    where v = lines input
          d = fmap ((\x -> (head x, (head . tail) x)) . transformInputLine)  v

transformInputLine :: String -> [Int]
transformInputLine a = read <$> words a

findDifference :: ([Int], [Int]) -> Int
findDifference  (a, b) = sum differences
    where grouped = zip (sort a) (sort b)
          differences = fmap (\(a, b) -> abs(a - b)) grouped

calculateScore :: [Int] -> Int -> Int
calculateScore ns n = n * length occurrences
    where occurrences = filter (== n) ns

similarityScore :: ([Int], [Int]) -> Int
similarityScore (a, b) = sum similarities
    where similarities = fmap (b `calculateScore`) a

main :: IO ()
main = do
    
    input <- getContents

    let formatedInput = extractData input
        response = findDifference formatedInput
        score = similarityScore formatedInput

    putStrLn $ "Distance between lists: " ++ show response
    putStrLn $ "Similarity score: " ++ show score

