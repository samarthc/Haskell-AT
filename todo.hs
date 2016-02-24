import System.IO
import Control.Monad

main = do
    newToDo <- getLine
    when (not . null $ newToDo) $ do
        appendFile "todo.txt" (newToDo ++ "\n")
        main