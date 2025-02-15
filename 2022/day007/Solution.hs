import Control.Applicative hiding (some)
import qualified Data.Map as M
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void String


data InputLine = 
    Cd String | Ls | DirName String | FileName String Int
        deriving (Show)



data FileSystem = FileSystem { dirStack :: [FileTree] }
    deriving (Show)


command :: Parser InputLine
command = string "$ " *> (ls <|> cd)
    where
        ls = string "ls" *> return Ls
        cd = string "cd " *> (Cd <$> (some printChar))


dir :: Parser InputLine
dir = string "dir " *> (DirName <$> (some printChar))


file :: Parser InputLine
file = do
    size <- L.decimal <* char ' '
    name <- some printChar

    return $ FileName name size


input :: Parser [InputLine]
input = sepEndBy1 (command <|> dir <|> file) eol


main :: IO ()
main = do
    inputData <- getContents

    let parsedData = parseTest input inputData

    parsedData
