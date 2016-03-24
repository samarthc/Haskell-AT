filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' _ [] = return []
filterM' mp (x:xs) = do
    pred <- mp x
    if pred then do
        rest <- filterM' mp xs
        return (x:rest)
    else
        filterM' mp xs

powerset :: [a] -> [[a]]
powerset = filterM' (\x -> [True,False])

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' monfunc = sequence' . map monfunc

sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (m:ms) = do
    result <- m
    rest <- sequence' ms
    return (result:rest)