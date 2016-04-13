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

parseExpr :: Parser LispVal
parseExpr = try parseSingleLineCommentExpr <|> try parseList <|> parseDottedList <|> try parseVector <|> try parseCharacter <|> parseString <|> try parseComplex <|> try parseFloat <|> try parseRatio <|> parseNumber <|> parseBool <|> parseQuoted <|> parseQuasiQuoted <|> parseUnQuote <|> parseAtom

readParametrized :: Parser a -> String -> Either LispError a
readParametrized parser input = case parse parser "" input of
    Left err -> throwError $ Parser err
    Right val -> return val

readExpr :: String -> Either LispError LispVal
readExpr = readParametrized (spaces >> parseExpr)

readExprList :: String -> Either LispError [LispVal]
readExprList = readParametrized (parseExpr `endBy` spacesOrComment)

symbol :: Parser Char
symbol = oneOf "~!@$%^&*-_=+<>?/:|"

spaces :: Parser ()
spaces = skipMany space

spacesOrComment :: Parser ()
spacesOrComment = try spaces <|> try parseSingleLineComment

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

parseSingleLineComment :: Parser ()
parseSingleLineComment = do
	char ';'
	skipMany $ noneOf "\n"

parseSingleLineCommentExpr :: Parser LispVal
parseSingleLineCommentExpr = do
	start <- char ';'
	comment <- many $ noneOf "\n"
	return $ Comment (start:comment)

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ Atom atom

parseNumber :: Parser LispVal
parseNumber = parseBin <|> parseOct <|> parseDecIndicated <|> parseHex <|> parseDecSimple

parseDecSimple :: Parser LispVal
parseDecSimple = do
    negative <- optionMaybe . try $ char '-'
    num <- fmap read $ many1 digit
    case negative of
        Just '-' -> return . Number . negate $ num
        Nothing -> return . Number $ num

parseDecIndicated :: Parser LispVal
parseDecIndicated = do
    try $ string "#d"
    parseDecSimple

parseHex :: Parser LispVal
parseHex = do
    try $ string "#x"
    negative <- optionMaybe . try $ char '-'
    num <- fmap hex2dec $ many1 hexDigit
    case negative of
        Just '-' -> return . Number . negate $ num
        Nothing -> return . Number $ num
    where
        hex2dec = fst . head . readHex

parseOct :: Parser LispVal
parseOct = do
    try $ string "#o"
    negative <- optionMaybe . try $ char '-'
    num <- fmap oct2dec $ many1 octDigit
    case negative of
        Just '-' -> return . Number . negate $ num
        Nothing -> return . Number $ num
    where
        oct2dec = fst . head . readOct

parseBin :: Parser LispVal
parseBin = do
    try $ string "#b"
    negative <- optionMaybe . try $ char '-'
    num <- fmap bin2dec . many1 $ oneOf "10"
    case negative of
        Just '-' -> return . Number . negate $ num
        Nothing -> return . Number $ num
    where
        bin2dec = bin2decAux 0
        bin2decAux num "" = num
        bin2decAux num (x:xs) = bin2decAux (2*num + read [x]) xs

parseFloat :: Parser LispVal
parseFloat = do
    negative <- optionMaybe . try $ char '-'
    x <- many1 digit
    char '.'
    y <- many1 digit
    case negative of
        Just '-' -> return . Float . negate . read $ x ++ "." ++ y
        Nothing -> return . Float . read $ x ++ "." ++ y

parseRatio :: Parser LispVal
parseRatio = do
    negative <- optionMaybe . try $ char '-'
    x <- many1 digit
    char '/'
    y <- many1 digit
    case negative of
        Just '-' -> return . Ratio . negate $ read x % read y
        Nothing -> return . Ratio $ read x % read y

parseComplex :: Parser LispVal
parseComplex = do
    x <- (try parseFloat <|> parseNumber)
    spaces
    char '+'
    spaces
    imag <- optionMaybe (try parseFloat <|> parseNumber)
    char 'i'
    case imag of
        Nothing -> return $ Complex (toDouble x :+ 1)
        Just y -> return $ Complex (toDouble x :+ toDouble y)
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
