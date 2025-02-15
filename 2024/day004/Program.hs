import Data.List (transpose)

find :: String -> String -> Int
find "" "" = 1
find _ "" = 0
find "" ys = 1 + find "XMAS" ys
find a@(x:xs) q@(y:ys)
    | x == y = find xs ys
    | y == 'X' = find "XMAS" q
    | otherwise = find "XMAS" ys

toInverse :: [String] -> [String]
toInverse xs = reverse <$> xs

toColumn :: [String] -> [String]
toColumn = transpose

toDiagonals :: [String] -> [String]
toDiagonals xs = rowDiagonals ++ columnDiagonals xs
    where rowDiagonals = fmap (diagonal xs) (enumFromTo 1 (length (head xs) - 1))
          columnDiagonals :: [String] -> [String]
          columnDiagonals [] = []
          -- columnDiagonals [x] = [diagonal [x] 0]
          columnDiagonals q@(x:xs) = diagonal q 0 : columnDiagonals xs

diagonal :: [String] -> Int -> String
diagonal xs i = func xs i
    where columns = length $ head xs
          func :: [String] -> Int -> String
          func [] c = []
          func (y:ys) c
            | c >= columns = []
            | otherwise = y !! c : func ys (c + 1)

main :: IO ()
main = do
    input <- lines <$> getContents

    let allData = concat 
            [ input
            , toColumn input
            , toDiagonals input
            , (toDiagonals . toInverse) input
            , toInverse input
            , (toInverse. toColumn) input
            , (toInverse . toDiagonals) input
            , (toInverse . toDiagonals . toInverse) input
            ]

    let matches = sum $ find "XMAS" <$> allData
    putStrLn $ "Matches: " ++ show matches
