import qualified Data.Map as M

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen f s = case dropWhile f s of
    "" -> []
    s' -> w : wordsWhen f s''
        where (w, s'') = break f s'

parseConstraints :: [String] -> [(Int, Int)]
parseConstraints xs = (\ x -> (head x, x !! 1)) <$> formated
    where separate :: [[String]]
          separate = wordsWhen (=='|') <$> xs
          formated :: [[Int]]
          formated = (fmap . fmap) read separate

parseQueries :: [String] -> [[Int]]
parseQueries xs = formated
    where separate :: [[String]]
          separate = wordsWhen (==',') <$> xs
          formated :: [[Int]]
          formated = (fmap . fmap) read separate

buildMap :: [(Int, Int)] -> M.Map Int [Int]
buildMap input = M.fromListWith (++) $ (fmap . fmap) (:[]) input

process :: M.Map Int [Int] -> [[Int]] -> Int
process t xs = sum $ fmap getMiddleElement valid
    where valid = filter (validate t) xs
          getMiddleElement :: [Int] -> Int
          getMiddleElement xs = xs !! quot (length xs) 2

validate :: M.Map Int [Int] -> [Int] -> Bool
validate _ [] = True
validate t (x:xs) = isValid t x xs && validate t xs

-- Checks that there is no element that sould go after the given
-- int in a list base on the constraints map.
isValid :: M.Map Int [Int] -> Int -> [Int] -> Bool
isValid _ _ [] = True
isValid t x (y:ys) = case M.lookup y t of
    Nothing -> isValid t x ys
    Just qs -> notElem x qs && isValid t x ys
            

main :: IO ()
main = do
    input <- lines <$> getContents

    let constraints = parseConstraints $ takeWhile (/= "") input
        queries = parseQueries $ drop 1 $ dropWhile (/= "") input
        constraintsMap = buildMap constraints
        result = process constraintsMap queries
    
    putStrLn $ "Sum of middle elements " ++ show result
