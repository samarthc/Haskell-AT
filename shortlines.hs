main = interact shortLines

shortLines :: String -> String
shortLines = unlines . filter (\a -> length a < 10) . lines
