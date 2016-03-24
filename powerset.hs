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