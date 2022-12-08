{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe
import Text.Trifecta

type Interval = (Int, Int)
type Line = (Interval, Interval)


contained :: Line -> Bool
contained (first, second) = a >= c && b <= d || c >= a && d <= b
    where (a, b) = first
          (c, d) = second


overlap :: Line -> Bool
overlap (first, second) = not (b < c || d < a)
    where (a, b) = first
          (c, d) = second


firstPart :: [Line] -> Int
firstPart lines = length $ filter contained lines


secondPart :: [Line] -> Int
secondPart lines = length $ filter overlap lines


interval :: Parser Interval
interval = do
    start <- integer
    _ <- char '-'
    end <- integer
    return (fromInteger start, fromInteger end)


intervalsLine :: Parser Line
intervalsLine = do
    intervalOne <- interval
    _ <- comma
    intervalTwo <- interval
    return (intervalOne, intervalTwo)


parseInput :: Parser [Line]
parseInput = some intervalsLine


main :: IO ()
main = do
    rawInput <- getContents
    let parsedInput = parseString parseInput mempty rawInput
    print $ firstPart <$> parsedInput
    print $ secondPart <$> parsedInput
