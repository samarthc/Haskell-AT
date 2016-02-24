import System.IO
import System.Directory

delete :: Int -> [a] -> [a]
delete 0 a = tail a
delete n (h:t) = [h] ++ delete (n-1) t

main = do
    toDoHandle <- openFile "todo.txt" ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents toDoHandle
    
    let tasks = lines contents
        numToDo = zipWith (\n task -> show n ++ " - " ++ task) [1..] tasks
    putStrLn "Here are your todos: "
    putStr $ unlines numToDo
    putStr "Which one do you want to delete? "
    hFlush stdout
    
    strNum <- getLine
    let num = read strNum
        modified = unlines $ delete (num-1) tasks
    
    hPutStr tempHandle modified
    
    hClose toDoHandle
    hClose tempHandle
    
    removeFile "todo.txt"
    renameFile tempName "todo.txt"
    