import Text.ParserCombinators.ReadP
import Data.Char
import Control.Monad

airport :: ReadP String
airport = do
    code <- many1 . satisfy $ isUpper
    satisfy (==' ')
    return code

type Timestamp = (Int, Int, Int)

timestamp :: ReadP Timestamp
timestamp = do
    day <- numbers 2
    guard $ day `elem` [1..31]
    hour <- numbers 2
    guard $ hour `elem` [0..23]
    minute <- numbers 2
    guard $ minute `elem` [0..59]
    string "Z "
    return (day, hour, minute)

numbers :: Int -> ReadP Int
numbers cnt = fmap read . count cnt . satisfy $ isDigit
