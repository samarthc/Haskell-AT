main = do
    putStrLn "Hello, user!"
    putStrLn "What is your name?"
    name <- getLine
    putStrLn $ "Hey, " ++ name ++ ". You rock!"