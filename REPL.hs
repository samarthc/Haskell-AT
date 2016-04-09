import LispVal
import LispError
import SchemeParsers
import SchemeEval
import Control.Monad
import Control.Applicative
import Data.List
import System.Environment
import System.IO

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return . extractValue . trapError . fmap show $  (readExpr expr) >>= eval

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    when (not $ pred result) $ do
        action result >> until_ pred prompt action

isQuit :: String -> Bool
isQuit input = case words input of
    ["quit"] -> True
    ["exit"] -> True
    otherwise -> False

runRepl :: IO ()
runRepl = until_ isQuit (readPrompt "Lisp>>> ") evalAndPrint


main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> runRepl
        otherwise -> forM_ args evalAndPrint
