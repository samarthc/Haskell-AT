reverse' :: [a] -> [a]
reverse' xs = foldl (\acc x -> x:acc) [] xs

foldr' :: (b -> a -> a) -> a -> [b] -> a
foldr' _ acc [] = acc
foldr' f acc (x:xs) = f x $ foldr' f acc xs