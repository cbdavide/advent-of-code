import Control.Monad.Combinators (some, optional)
import Data.List (inits, tails)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parse)
import Text.Megaparsec.Char (char, string, space)

import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String
type Operator = Int -> Int -> Int
data Line = Line Int [Int] deriving (Show)

operators :: [Operator]
operators = [(+), (*)]

operatorsWithConcat :: [Operator]
operatorsWithConcat = concatenate : operators 

concatenate :: Int -> Int -> Int 
concatenate a b = read (show a ++ show b)

parseLine :: Parser Line
parseLine = do
    result <- L.decimal
    _ <- string ": "
    operands <- some (L.decimal <* optional space)
    return $ Line result operands

parseInput :: [String] -> [Line]
parseInput = fmap func
    where func :: String -> Line
          func x = case parse parseLine "input" x of
            Left e -> undefined -- we are not expecting a bad input
            Right v -> v

-- Get all the possible combinations with repetitions
-- of n elements for the given set of operators
combine :: [Operator] -> Int -> [[Operator]]
combine _ 0 = [[]]
combine o n = [ x:rest | x <- o, rest <- combine o (n - 1)]

-- Applies the operators to the input list. Evaluates
-- the numers left to right.
evaluate :: [Int] -> [Operator] -> Int -> Int
evaluate [] _ acc = acc
evaluate (x:xs) (o:ops) acc = evaluate xs ops (o acc x)

-- There is a combintation of operators that make the 
-- elements of the list to be equal to the result
isValid :: [Operator] -> Line -> Bool
isValid ops (Line result xs) = func xs (combine ops (length xs - 1))
    where tryCmb :: [Int] -> [Operator] -> Int
          tryCmb [x] ops = x -- single element no need to evaluate
          tryCmb xs ops = evaluate (tail xs) ops (head xs)
          func :: [Int] -> [[ Operator ]] -> Bool
          func xs [] = False -- operators combination exhausted 
          func xs (o:ops) = tryCmb xs o == result || func xs ops

process :: (Line -> Bool) -> [Line] -> Int
process f xs = sum (extract <$> filter f xs)
    where extract (Line v _) = v

main :: IO ()
main = do
    input <- getContents
    let _lines = parseInput $ lines input
        response = process (isValid operators) _lines
        badInputs = filter (not . isValid operators) _lines
        badInputsResponse = process (isValid operatorsWithConcat) badInputs

    putStrLn $ "Sum of valid results: " ++ show response
    putStrLn $ "Sum of bad results: " ++ show (badInputsResponse + response)
