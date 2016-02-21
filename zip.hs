zip' :: [a] -> [b] -> [(a, b)]

zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : (zip' xs ys)

unzip' :: [(a,b)] -> ([a], [b])

unzip' [] = ([], [])
unzip' ((a,b):xs) = (a:(fst (unzip' xs)), b:(snd (unzip' xs)))