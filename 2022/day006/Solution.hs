import qualified Data.Set as S


check :: Int -> String -> Bool
check n xs = length (S.fromList xs) == n


solution :: Int -> Int -> String -> String -> Int
solution k n xs (y:ys)
    | check k xs = n
    | otherwise = solution k (n + 1) (y:take (k - 1) xs) ys


main :: IO ()
main = do
    content <- head . lines <$> getContents
    print $ solution 4 4 (reverse $ take 4 content) (drop 4 content)
    print $ solution 14 14 (reverse $ take 14 content) (drop 14 content)
