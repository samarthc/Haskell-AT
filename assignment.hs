import Data.List (groupBy, transpose)

-- 1. Programming with lists
-- 1.1. Take 
take' :: (Integral n) => n -> [a] -> [a]
take' _ [] = []
take' 0 _ = []
take' n (x:xs) = x : take' (n-1) xs

-- 1.2. Drop
drop' :: (Integral n) => n -> [a] -> [a]
drop' _ [] = []
drop' 0 xs = xs
drop' n (x:xs) = drop' (n-1) xs

-- 1.3. Merge
merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge l1@(x:xs) l2@(y:ys)
    | x > y = y : merge l1 ys
    | otherwise = x : merge xs l2

-- 2. Higher-Order Programming
-- 2.1. ZipWith
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ xs [] = []
zipWith' _ [] ys = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- 2.2. Map using foldr
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- 2.3. Foldl
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ acc [] = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs

-- 3. Lazy Programming
-- 3.1. Taylor Series
fac :: (Integral n) => n -> n
fac n = product [1..n]

--sinNth generates the nth term in the Taylor series for sin(x) at a given x
sinNth :: (Floating x, Integral n) => x -> n -> x
sinNth x n = (if odd n then 1 else (-1) ) * x^(2*n - 1) / (fromIntegral (fac $ 2*n - 1))

-- sinTaylor returns the terms in the Taylor series for sin(x) at a given x
sinTaylor :: (Floating n) => n -> [n]
sinTaylor x = map (sinNth x) [1..]

-- 3.2. Termination Criteria
-- 3.2.1.
sinFirstN :: (Floating x) => x -> Int -> x
sinFirstN x n = sum . take n . sinTaylor $ x

-- 3.2.2.
takeWithPrecision :: (Num a, Ord a) => a -> [a] -> [a]
takeWithPrecision _ [] = []
takeWithPrecision _ [x] = [x]
takeWithPrecision e (f:s:rest) = if abs(f-s) > e then f : takeWithPrecision e (s:rest) else [f]

sinWithPrecision :: (Floating a, Ord a) => a -> a -> [a]
sinWithPrecision e x = takeWithPrecision e $ sinTaylor x

-- 3.3. Tic-Tac-Toe
-- 3.3.1.
-- diag1 generates the principal diagonal of a square matrix
diag1 :: [[a]] -> [a]
diag1 [] = []
diag1 [x] = [head x]
diag1 (x:xs) = head x : diag1 (map tail xs)

-- diag2 generates the other diagonal
diag2 :: [[a]] -> [a]
diag2 [] = []
diag2 [x] = [last x]
diag2 (x:xs) = last x : diag2 (map init xs)

-- side note: diag2 could be written as diag2 = reverse . diag1 . reverse to make it more efficient, but since
-- the grid size here would be small (3x3) I chose to sacrifice a small upgrade in speed for additional clarity

diagonals :: [[a]] -> [[a]]
diagonals ls = [diag1 ls, diag2 ls]

-- won is True iff the person playing c (which could be either 'x' or 'o') has won
won :: Char -> [[Char]] -> Bool
won c grid = [c,c,c] `elem` (grid ++ (transpose grid) ++ (diagonals grid))

result :: [[Char]] -> String
result grid
    | won 'o' grid = "o"
    | won 'x' grid = "x"
    | otherwise = "draw"

-- 3.3.2.
-- nextRow generates a list of all possible rows created by replacing exactly one 's' by ch
nextRow :: Char -> [Char] -> [[Char]]
nextRow _ [] = []
nextRow ch (c:cs) = filter (not.null) $ (if c == 's' then ch : cs else []) : (map (c:) $ nextRow ch cs)

-- nextGrid generates a list of all possible grids created by replacing exactly one 's' by ch
nextGrid ::  Char -> [[Char]] -> [[[Char]]]
nextGrid ch (r1:[]) = map (:[]) $ nextRow ch r1
nextGrid ch (r1:rest) = map (:rest) (nextRow ch r1) ++ (map (r1:) $ nextGrid ch rest)
