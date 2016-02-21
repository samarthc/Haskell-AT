zipWith1 :: (a  -> b -> c) -> [a] -> [b] -> [c]

zipWith1 f xs ys= [ f x y | i <- [0..(min (length xs) (length ys)) - 1], let x = xs!!i; y = ys!!i]


zipWith2 :: (a -> b -> c) -> [a] -> [b] -> [c]

zipWith2 _ _ [] = []
zipWith2 _ [] _ = []
zipWith2 f (x:xs) (y:ys) = (f x y) : (zipWith2 f xs ys)