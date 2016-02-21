foldl' :: (a -> b -> a) -> a -> [b] -> a

foldl' _ acc [] = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs

length' :: [a] -> Int

length' = foldl' (\acc _ -> succ acc) 0