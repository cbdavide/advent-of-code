-- A, X: Rock 1
-- B, Y: Paper 2
-- C, Z: Scissors 3


data Play = A | B | C deriving (Eq, Show, Read)
data Code = X | Y | Z deriving (Eq, Show, Read)

type Round = (Play, Play)


calcRoundScore :: Play -> Play -> Int
calcRoundScore A B = 6
calcRoundScore B C = 6
calcRoundScore C A = 6
calcRoundScore a b = if a == b then 3 else 0


getValue :: Play -> Int
getValue A = 1
getValue B = 2
getValue C = 3


evaluate :: Round -> Int
evaluate (a, b) = calcRoundScore a b + getValue b


calculateScore :: [Round] -> Int
calculateScore rounds = sum $ fmap evaluate rounds


parseInput :: [String] -> [(Play, Code)]
parseInput lines = fmap parseTuple $ fmap (splitAt 1) lines
    where parseTuple (a, b) = (read a, read b)


firstPart :: [(Play, Code)] -> Int
firstPart xs = calculateScore $ fmap getPlayFromCode xs
    where
        getPlayFromCode :: (Play, Code) -> Round
        getPlayFromCode (a, X) = (a, A)
        getPlayFromCode (a, Y) = (a, B)
        getPlayFromCode (a, Z) = (a, C)


secondPart :: [(Play, Code)] -> Int
secondPart xs = calculateScore $ fmap getPlayFromCode xs
    where
        getPlayFromCode :: (Play, Code) -> Round
        getPlayFromCode (a, X) = (a, getLosePlay a)
        getPlayFromCode (a, Y) = (a, a)
        getPlayFromCode (a, Z) = (a, getWinPlay a)

        getWinPlay :: Play -> Play
        getWinPlay A = B
        getWinPlay B = C
        getWinPlay C = A

        getLosePlay :: Play -> Play
        getLosePlay A = C
        getLosePlay B = A
        getLosePlay C = B


main :: IO ()
main = do
    input <- getContents
    let parsedInput = parseInput $ lines input
    print $ firstPart parsedInput
    print $ secondPart parsedInput
