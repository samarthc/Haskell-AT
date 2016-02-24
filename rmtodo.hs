import System.IO
import System.Directory
import Data.List

main = do
    toDoHandle <- openFile "todo.txt" ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents toDoHandle
    
    let tasks = lines contents
        numToDo = zipWith (\n task -> show n ++ " - " ++ task) [1..] tasks
    putStrLn "Here are your todos: "
    putStr $ unlines numToDo
    putStrLn "Which one do you want to delete? "
    
    strNum <- getLine
    let num = read strNum
        delToDo = tasks!!(num-1)
        modified = unlines $ delete delToDo tasks
    
    hPutStr tempHandle modified
    
    hClose toDoHandle
    hClose tempHandle
    
    removeFile "todo.txt"
    renameFile tempName "todo.txt"
    