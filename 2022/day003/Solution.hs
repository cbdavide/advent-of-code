import Data.Char
import Data.String
import qualified Data.Set as S


chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)


getPriority :: Char -> Int
getPriority a
    | isUpper a = ord a - 64 + 26
    | otherwise = ord a - 96


getUnorderedElement :: String -> Char
getUnorderedElement input = S.elemAt 0 $ S.intersection first second
    where 
        mid = length input `div` 2
        (a, b) = splitAt mid input
        first = S.fromList a
        second = S.fromList b


getBatch :: [String] -> Char
getBatch xs = S.elemAt 0 $ foldr1 S.intersection sets
    where sets = fmap S.fromList xs


firstPart :: [String] -> Int
firstPart xs = sum $ fmap (getPriority . getUnorderedElement) xs


secondPart :: [String] -> Int
secondPart xs = sum $ fmap (getPriority . getBatch) chunks
    where chunks = chunksOf 3 xs


main :: IO ()
main = do
    input <- getContents
    print $ firstPart $ lines input
    print $ secondPart $ lines input
