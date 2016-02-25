import System.IO
import System.Directory
import System.Environment
import Data.List (lookup)

delete :: Int -> [a] -> [a]
delete 0 a = tail a
delete n (h:t) = [h] ++ delete (n-1) t

dispatch :: [(String, [String] -> IO ())]
dispatch = [("add", add) , ("view", view), ("remove", remove)]

main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

add :: [String] -> IO ()
add [path, toAppend] = do
    appendFile path (toAppend ++ "\n")

view :: [String] -> IO ()
view [path] = do
    contents <- readFile path
    let tasks = lines contents
        numTasks = zipWith (\n task -> show n ++ " - " ++ task) [1..] tasks
    putStr $ unlines numTasks

remove :: [String] -> IO ()
remove [path, strNum] = do
    handle <- openFile path ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle

    let num = read strNum
        tasks = lines contents
        modified = delete (num - 1) tasks
    
    hPutStr tempHandle $ unlines modified
    
    hClose tempHandle
    hClose handle
    
    removeFile path
    renameFile tempName path