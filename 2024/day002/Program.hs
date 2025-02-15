parseLevel :: String -> [Int]
parseLevel a = read <$> words a

getLevels :: String -> [[Int]]
getLevels input = parseLevel <$> lines input

validate :: (Int -> Int -> Bool) -> [Int] -> Bool
validate _ [] = True
validate _ [x]= True
validate cmp (x : y : ys) = cmp x y && validate cmp (y : ys)

distanceCmp :: Int -> Int -> Bool
distanceCmp a b = distance >= 1 && distance <= 3
    where distance = abs (a - b)

check :: [Int] -> Bool
check as = (validate (>) as || validate (<) as) && validate distanceCmp as

checkRemoval :: [Int] -> [Int] -> Bool
checkRemoval a [] = False
checkRemoval a (x:xs) = check (a ++ xs) || checkRemoval (a ++ [x]) xs

filterSafeLevels :: [[Int]] -> [[Int]]
filterSafeLevels = filter check 

filterSafeLevelsWithRemoval :: [[Int]] -> [[Int]]
filterSafeLevelsWithRemoval = filter check'
    where check' :: [Int] -> Bool
          check' as = check as || checkRemoval [] as

main :: IO ()
main = do
    input <- getContents
    
    let levels = getLevels input
        safeLevels = filterSafeLevels levels
        safeLevelsWithRemoval = filterSafeLevelsWithRemoval levels

    putStrLn $ "Safe levels count: " ++ show (length safeLevels)
    putStrLn $ "Safe levels with removal count: " ++ show (length safeLevelsWithRemoval)
