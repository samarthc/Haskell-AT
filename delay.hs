a = map print . iterate (2^) $ 1
main = do
    line <- getLine
    if null line
        then do
            head a
            let a = tail a
            main
        else
            return ()