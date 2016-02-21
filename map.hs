findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey _ [] = Nothing
findKey k ((x,y):xs) = if x == k then Just y else findKey k xs

findKey2 :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey2 k = foldr (\(x,y) acc -> if x == k then Just y else acc) Nothing