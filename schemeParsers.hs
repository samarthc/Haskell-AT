import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Numeric (readHex, readOct)
import Control.Applicative (Applicative(..))
import Data.Char
import Data.Complex
import Data.Ratio

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Double
             | Ratio Rational
             | Complex (Complex Double)
             | Character Char
             | String String
             | Bool Bool deriving (Show, Eq)

symbol :: Parser Char
symbol = oneOf "~!@$%^&*-_=+<>?/:|"

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ Atom atom

parseNumber :: Parser LispVal
parseNumber = parseBin <|> parseOct <|> parseDecIndicated <|> parseHex <|> parseDecSimple

parseDecSimple :: Parser LispVal
parseDecSimple = fmap (Number . read) $ many1 digit

parseDecIndicated :: Parser LispVal
parseDecIndicated = do
    try $ string "#d"
    fmap (Number . read) $ many1 digit

parseHex :: Parser LispVal
parseHex = do
    try $ string "#x"
    fmap (Number . hex2dec) $ many1 hexDigit
    where
        hex2dec = fst . head . readHex

parseOct :: Parser LispVal
parseOct = do
    try $ string "#o"
    fmap (Number . oct2dec) $ many1 octDigit
    where
        oct2dec = fst . head . readOct

parseBin :: Parser LispVal
parseBin = do
    try $ string "#b"
    fmap (Number . bin2dec) $ many1 (oneOf "10")
    where
        bin2dec = bin2decAux 0
        bin2decAux num "" = num
        bin2decAux num (x:xs) = bin2decAux (2*num + read [x]) xs

parseFloat :: Parser LispVal
parseFloat = do
    x <- many1 digit
    char '.'
    y <- many1 digit
    return . Float . read $ x ++ "." ++ y

parseRatio :: Parser LispVal
parseRatio = do
    x <- many1 digit
    char '/'
    y <- many1 digit
    return . Ratio $ read x % read y

parseComplex :: Parser LispVal
parseComplex = do
    x <- (try parseFloat <|> parseNumber)
    try (string "+") <|> try (string " + ")
    y <- (try parseFloat <|> parseNumber)
    char 'i'
    return $ Complex (toDouble x :+ toDouble y)
    where
        toDouble (Float f) = realToFrac f
        toDouble (Number n) = fromIntegral n

parseCharacter :: Parser LispVal
parseCharacter = do
    try $ string "#\\"
    value <- try (stringi "newline" <|> stringi "space") <|> do { x <-    anyChar; notFollowedBy alphaNum; return [x]}
    return $ Character $ case value of
        "space" -> ' '
        "newline" -> '\n'
        _ -> head value

-- | Parser that matches a string without matching case
stringi :: String -> Parser String
stringi "" = return ""
stringi (x:xs) = do
    first <- oneOf $ [toUpper, toLower] <*> [x]
    rest <- stringi xs
    return $ first:rest

escapedChars :: Parser Char
escapedChars = do
    char '\\'
    x <- oneOf "\\\"\n\r\t"
    return $ case x of
        'n' -> '\n'
        't' -> '\t'
        'r' -> '\r'
        _ -> x
    
parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many $ escapedChars <|> noneOf "\"\\\n\t\r"
    char '"'
    return $ String x

parseBool :: Parser LispVal
parseBool = do
    char '#'
    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool True))

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseCharacter <|> parseString <|> try parseFloat <|> try parseRatio <|> try parseComplex <|> parseNumber <|> parseBool

spaces :: Parser ()
spaces = skipMany space

readExpr :: String -> String
readExpr input = case parse (spaces >> parseExpr) "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

main :: IO ()
main = do
    args <- getArgs
    case args of
        (expr:_) -> putStrLn . readExpr $ expr
        [] -> do
            expr <- getLine
            putStrLn . readExpr $ expr
