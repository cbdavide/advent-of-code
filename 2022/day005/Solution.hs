{-# LANGUAGE RecordWildCards #-}

import Control.Applicative hiding (some)

import Data.Either
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void String

type Crate = Maybe Char
type CrateStacks = M.Map Int [Crate]


data Instruction = Instruction { cont :: Int, from :: Int, to :: Int}
    deriving (Show)


data Input = Input 
    { _cargo :: [[Crate]], _ids :: [Int], _instructions :: [Instruction] }
    deriving (Show)


crate :: Parser Crate
crate = empty <|> nonEmpty
    where
        empty = string "   " >> pure Nothing
        nonEmpty = Just <$> ((char '[') *> upperChar <* (char ']'))


cargo :: Parser [[Crate]]
cargo = sepEndBy1 cargoLine newline
    where cargoLine = sepBy1 crate (char ' ') 


cargoIdentifiers :: Parser [Int]
cargoIdentifiers = 
    (some (skipMany (char ' ') *> L.decimal <* skipMany (char ' '))) <* eol


instruction :: Parser Instruction
instruction = do
    _ <- string "move "
    cont <- L.decimal
    _ <- string " from "
    from <- L.decimal
    _ <- string " to "
    to <- L.decimal
    return $ Instruction cont from to


input :: Parser Input
input = do
    lines <- cargo
    ids <-  cargoIdentifiers <* eol
    instructions <- sepEndBy1 instruction newline
    return $ Input lines ids instructions


buildCratesStack :: [Int] -> [[Crate]] -> CrateStacks
buildCratesStack ids crateLines = M.fromList $ zip ids filteredCrates
    where orderedCrates = L.transpose crateLines
          filteredCrates = fmap (filter isJust) orderedCrates


processInstruction :: ([Crate] -> [Crate]) -> CrateStacks -> Instruction -> CrateStacks
processInstruction f crateStacks Instruction {..} = 
    M.insert to newToStack (M.insert from newFromStack crateStacks)
        where 
            toStack = M.findWithDefault [] to crateStacks
            fromStack = M.findWithDefault [] from crateStacks

            newFromStack = drop cont fromStack
            newToStack = (f $ take cont fromStack) ++ toStack


solution :: ([Crate] -> [Crate]) -> CrateStacks -> [Instruction] -> [Char]
solution f crateStacks instructions = formatedSolution
    where 
        solution = foldl (processInstruction f) crateStacks instructions
        formatedSolution = M.foldr (\a b -> (fromJust . head) a : b) [] solution


partOne :: CrateStacks -> [Instruction] -> [Char]
partOne = solution reverse


partTwo :: CrateStacks -> [Instruction] -> [Char]
partTwo = solution id


main :: IO ()
main = do
    rawInput <- getContents

    let parsedInput = parse input "input" rawInput
        inputData = fromRight undefined parsedInput
        crateStacks = buildCratesStack (_ids inputData) (_cargo inputData)

    putStrLn $ partOne crateStacks (_instructions inputData)
    putStrLn $ partTwo crateStacks (_instructions inputData)
