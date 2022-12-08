import Data.List


parseInput :: String -> [[String]]
parseInput input = process $ lines input
    where 
        process [] = []
        process xs = takeWhile (/= "") xs :
            process (dropWhile (== "") (dropWhile (/= "") xs))


firstPart :: [Int] -> Int
firstPart = foldr max 0


secondPart :: [Int] -> Int
secondPart values = sum . take 3 $ reverse . sort $ values


main :: IO ()
main = do
    input <- getContents

    let plain = parseInput input
        parsed = (fmap . fmap) read plain :: [[Int]]

        sums = sum <$> parsed

    print $ firstPart sums
    print $ secondPart sums
