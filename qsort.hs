qsort :: (Ord a) => [a] -> [a]

qsort [] = []
qsort (x:xs) = qsort lessX ++ [x] ++ greatX
    where lessX = [y | y <- xs, y <= x]
          greatX = [y | y <- xs, y > x]