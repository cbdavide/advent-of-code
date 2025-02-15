import Data.Char (digitToInt)
import GHC.Exts (fromList)
import qualified Deque.Strict as D

data Element = File Int Int | Empty Int deriving (Show)

parseInput :: String -> [Element]
parseInput xs = func (digitToInt <$> xs) True 0
    where func :: [Int] -> Bool -> Int -> [Element]
          func [] _ _ = []
          func (x:xs) flag cnt
            | flag = File x cnt : func xs (not flag) (cnt + 1)
            | otherwise = Empty x : func xs (not flag) cnt


process :: D.Deque Element -> [Int]
process ds  
    | null ds = []
    | otherwise = process' ds

process' :: D.Deque Element -> [Int]
process' ds = case getHeadAndTail ds of
    Nothing -> case processLast ds of
        Nothing -> []
        (Just (n, id)) -> replicate n id
    (Just (a, b, xs)) -> processFirstAndLast a b xs

processLast :: D.Deque Element -> Maybe (Int, Int)
processLast ds = do
    (head, _) <- D.uncons ds

    case head of 
        (Empty _) -> Nothing
        (File n id) -> Just (n, id)

processFirstAndLast :: Element -> Element -> D.Deque Element -> [Int]
processFirstAndLast front (Empty n) xs = process (D.cons front xs)
processFirstAndLast (Empty m) (File n id) xs
    -- add m - n of file then add (m - n) to the front of the deque
    | m > n = replicate n id ++ process (D.cons (Empty (m - n)) xs)
    -- no need to add head and tail back 
    | m == n = replicate m id ++ process xs
    -- add n - m of file then add n - m to the back of the deque
    | otherwise = replicate m id ++ process (D.snoc (File (n - m) id) xs)
processFirstAndLast (File n id) back xs = replicate n id ++ process (D.snoc back xs)

getHeadAndTail :: D.Deque a -> Maybe (a, a, D.Deque a)
getHeadAndTail ds = do
    (head, ts) <- D.uncons ds
    (tail, xs) <- D.unsnoc ts
    return (head, tail, xs)

calculateChecksum :: [Int] -> Int
calculateChecksum xs = sum [x * y | (x, y) <- zip xs [0..]]

main :: IO ()
main = do
    input <- takeWhile (/= '\n') <$> getContents

    let parsedInput = parseInput input
        processed = process (fromList parsedInput)

    putStrLn $ "Checksum: " ++ show (calculateChecksum processed)
