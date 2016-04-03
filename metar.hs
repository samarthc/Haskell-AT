import Text.ParserCombinators.ReadP
import Data.Char
import Control.Monad
import Control.Applicative

data Timestamp = Timestamp {day :: Int, hour :: Int, minute :: Int} deriving Show

data WindInfo = WindInfo {dir :: Int, speed :: Int, gusts :: Maybe Int} deriving Show

data Report = Report {station :: String, time :: Timestamp, wind :: WindInfo} deriving Show

metar :: ReadP Report
metar = do
    code <- airport
    time <- timestamp
    wind <- windInfo
    return $ Report code time wind

airport :: ReadP String
airport = do
    code <- many1 . satisfy $ isUpper
    satisfy (==' ')
    return code

timestamp :: ReadP Timestamp
timestamp = do
    day <- numbers 2
    guard $ day `elem` [1..31]
    hour <- numbers 2
    guard $ hour `elem` [0..23]
    minute <- numbers 2
    guard $ minute `elem` [0..59]
    string "Z "
    return $ Timestamp day hour minute

numbers :: Int -> ReadP Int
numbers cnt = fmap read . count cnt . satisfy $ isDigit

windInfo :: ReadP WindInfo
windInfo = do
    direction <- numbers 3
    speed <- numbers 2 <|> numbers 3
    gusts <- option Nothing (fmap Just gust)
    unit <- string "KT" <|> string "MPS"
    string " "
    return $ WindInfo direction (toMPS unit speed) (fmap (toMPS unit) gusts)

toMPS :: String -> Int -> Int
toMPS unit speed = case unit of
    "KT" -> speed `div` 2
    "MPS" -> speed

gust :: ReadP Int
gust = do
    satisfy (== 'G')
    numbers 2 <|> numbers 3

