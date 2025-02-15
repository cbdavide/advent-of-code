import Data.Char (isDigit)

notComma x = x /= ','
notClosing x = x /= ')'
notCommaAndNotClosing x = notComma x && notClosing x

-- Possible inputs
-- mul(a,b) -> Just (a, b)
-- mul(a) -> Nothing
-- mul(a,b,c,.....) -> Nothing
extractMultipliers :: String -> Maybe (Int, Int)
extractMultipliers xs = if number2 == "" || final /= ")"
        then Nothing 
        else Just (read number1, read number2)

    where start = drop 4 xs
          number1 = takeWhile notCommaAndNotClosing start
          remaining = drop 1 $ dropWhile notCommaAndNotClosing start

          number2 = takeWhile notCommaAndNotClosing remaining
          final = dropWhile notCommaAndNotClosing remaining

getExpressions :: String -> String -> [String]
getExpressions [] _ = []
getExpressions ('d':'o':'(':')':xs) [] = "do()" : getExpressions xs []
getExpressions ('d':'o':'n':'\'':'t':'(':')':xs) [] = "don't()" : getExpressions xs []
getExpressions (x:xs) [] =
    if x == 'm' then getExpressions xs [x]
    else getExpressions xs []
getExpressions (x:xs) t@(y:_) 
    -- We reached the end of a valid expression we need to 
    -- concatenate and start again
    | x == ')' && validate x y = reverse (x:t) : getExpressions xs []
    | validate x y = getExpressions xs (x:t)
    -- Invalid new char we need to delete the whole expression
    | otherwise = getExpressions (x:xs) []

filterExpressions :: [String] -> [String]
filterExpressions [] = [] 
filterExpressions (x:xs)
    | x == "do()" = filterExpressions xs
    | x == "don't()" = filterExpressions $ dropWhile (/= "do()") xs
    | otherwise = x : filterExpressions xs


validate :: Char -> Char -> Bool
validate 'u' 'm' = True
validate 'l' 'u' = True
validate '(' 'l' = True
validate n o 
    | isDigit n && o == '(' = True
    | isDigit n && isDigit o = True
    | n == ',' && isDigit o = True
    | isDigit n && o == ',' = True
    | n == ')' && isDigit o = True
    | otherwise = False


multiply :: Maybe (Int, Int) -> Int
multiply Nothing = 0
multiply (Just (a, b)) = a * b

processLine :: String -> Int
processLine d = sum (multiply . extractMultipliers <$> expressions)
    where expressions = filterExpressions $ getExpressions d []

main :: IO ()
main = do
    input <- getContents

    let response = processLine input
        multiplies = getExpressions input []
        filtered = filterExpressions multiplies
        extracted = extractMultipliers <$> multiplies

    -- mapM_ putStrLn multiplies
    -- putStrLn $ "Exprs: " ++ show multiplies
    -- putStrLn $ "Filtered: " ++ show filtered
    -- putStrLn $ "Expressions: " ++ show extracted
    putStrLn $ "Multiplications: " ++ show response
