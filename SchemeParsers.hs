module SchemeParsers where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment (getArgs)
import Numeric (readHex, readOct)
import Control.Applicative (Applicative(..))
import Control.Monad
import Control.Monad.Error
import Data.List (lines, unwords)
import Data.Char (toUpper, toLower)
import Data.Ratio
import Data.Complex
import Data.Array
import LispVal
import LispError
import SchemeEval

symbol :: Parser Char
symbol = oneOf "~!@$%^&*-_=+<>?/:|"

spaces :: Parser ()
spaces = skipMany space

-- | Parser that matches a string without matching case
stringi :: String -> Parser String
stringi "" = return ""
stringi (x:xs) = do
    first <- fmap toLower . oneOf $ [toUpper, toLower] <*> [x]
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
    spaces
    char '+'
    spaces
    y <- (try parseFloat <|> parseNumber)
    char 'i'
    return $ Complex (toDouble x :+ toDouble y)
    where
        toDouble (Float f) = realToFrac f
        toDouble (Number n) = fromIntegral n

parseCharacter :: Parser LispVal
parseCharacter = do
    string "#\\"
    value <- do {x <- try (stringi "newline" <|> stringi "space") <|> fmap (:[]) anyChar; notFollowedBy alphaNum; return x}
    return $ Character $ case value of
        "space" -> ' '
        "newline" -> '\n'
        _ -> head value
    
parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many $ escapedChars <|> noneOf "\"\\\n\t\r"
    char '"'
    return $ String x

parseBool :: Parser LispVal
parseBool = do
    char '#'
    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseList :: Parser LispVal
parseList = do
    char '('
    spaces
    list <- parseExpr `sepBy` spaces
    char ')'
    spaces
    return $ List list

parseDottedList :: Parser LispVal
parseDottedList = do
    char '('
    spaces
    head <- parseExpr `endBy` spaces
    char '.'
    spaces
    tail <- parseExpr
    char ')'
    return $ DottedList head tail

parseVector :: Parser LispVal
parseVector = do
    string "#("
    arrayValues <- parseExpr `sepBy` spaces
    char ')'
    return $ Vector (listArray (0, length arrayValues - 1) arrayValues)

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
    char ','
    x <- parseExpr
    return $ List [Atom "unquote", x]

parseExpr :: Parser LispVal
parseExpr = try parseList <|> parseDottedList <|> parseAtom <|> try parseVector <|> try parseCharacter <|> parseString <|> try parseFloat <|> try parseRatio <|> try parseComplex <|> parseNumber <|> parseBool <|> parseQuoted <|> parseQuasiQuoted <|> parseUnQuote

readExpr :: String -> Either LispError LispVal
readExpr input = case parse (spaces >> parseExpr) "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

withoutArgs :: IO ()
withoutArgs = do
    fmap (:[]) getLine >>= \x -> case x of
                                [""] -> return ()
                                otherwise -> do
                                                withArgs x
                                                withoutArgs    

withArgs :: [String] -> IO ()
withArgs [] = return ()
withArgs [""] = return ()
withArgs (arg:rest) = do
    putStrLn . extractValue . trapError $ (readExpr arg >>= eval >>= (return . show))
    withArgs rest

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> withoutArgs
        _ -> withArgs args
