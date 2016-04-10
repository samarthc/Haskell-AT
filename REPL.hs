import LispVal
import LispError
import SchemeParsers
import SchemeEval
import SchemeEnv
import Control.Monad
import Control.Applicative
import Data.List
import System.Environment
import System.IO

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> runRepl
        otherwise -> do
            env <- nullEnv
            forM_ args $ evalAndPrint env 

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOError . fmap show $  (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

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
runRepl = nullEnv >>= until_ isQuit (readPrompt "Lisp>>> ") . evalAndPrint
