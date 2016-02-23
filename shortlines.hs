main = do
    contents <- getContents
    putStr shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter (\a -> length a < 10) . lines $ contents
