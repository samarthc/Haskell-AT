f :: Int -> Int -> [String]
f 0 0 = ["()"]
f l1 l2 = do
    w1 <- paren l1
    w2 <- paren l2
    return $ "(" ++ w1 ++ ")" ++ w2

paren :: Int -> [String]
paren 0 = [""]
paren n = concat $ do
    x <- [1..n]
    return $ f (n-x) (x-1)

main = do
    x <- fmap read getLine
    mapM_ putStrLn $ paren x