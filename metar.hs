import Text.ParserCombinators.ReadP
import Data.Char

airport :: ReadP String
airport = do
    code <- many1 . satisfy $ isUpper
    satisfy (==' ')
    return code

timestamp :: ReadP (Int, Int, Int)
timestamp = do
    day <- count 2 $ satisfy isDigit
    hour <- count 2 $ satisfy isDigit
    minute <- count 2 $ satisfy isDigit
    string "Z "
    return (read day, read hour, read minute)
