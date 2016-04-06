import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

symbol :: Parser Char
symbol = oneOf "~!@#$%^&*-_=+<>?/:|"

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

spaces :: Parser ()
spaces = skipMany space

main :: IO ()
main = do
    args <- getArgs
    case args of
        (expr:_) -> putStrLn . readExpr $ expr
        [] -> do
            expr <- getLine
            putStrLn . readExpr $ expr
