import LispVal
import SchemeParsers
import SchemeEval
import SchemeEnv
import SchemeInit
import Control.Monad
import Control.Monad.Error
import Control.Applicative
import Data.List
import System.Console.Haskeline
import System.Environment
import System.IO

main :: IO ()
main = do
    args <- getArgs
    env <- primitiveBindings
    case args of
        [] -> runRepl env
        [filename] -> evalAndPrint env (Just $ "(load \"" ++ filename ++"\")")  >> runRepl env
        otherwise -> do
            putStrLn "Expected just one filename; no files loaded"
            runRepl env

readPrompt :: String -> IO (Maybe String)
readPrompt prompt = runInputT defaultSettings $ getInputLine prompt

evalString :: Env -> Maybe String -> IO String
evalString env Nothing = return ""
evalString env (Just "") = return ""
evalString env (Just expr) = runIOError . fmap show $  (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> Maybe String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: (a -> Bool) -> IO a -> (a -> IO ()) -> IO ()
until_ pred prompt action = do
    result <- prompt
    if (pred result) 
        then putStrLn "Moriturus te saluto"
        else action result >> until_ pred prompt action

isQuit :: Maybe String -> Bool
isQuit input = case fmap words input of
    Just ["quit"] -> True
    Just ["exit"] -> True
    otherwise -> False

runRepl :: Env -> IO ()
runRepl env = until_ isQuit (readPrompt "Lisp>>> ") $ evalAndPrint env
