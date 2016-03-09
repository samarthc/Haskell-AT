module MyList
( intersperse''
, intercalate''
, concat''
, concatMap''
, and''
, iterate''
, splitAt''
, takeWhile''
, dropWhile''
, span''
, break''
, group''
, groupBetter
, partition''
, lines''
, unlines''
, words''
, unwords''
, split''
) where

intersperse'' :: a -> [a] -> [a]
intersperse'' _ [] = []
intersperse'' a [x] = [x]
intersperse'' a (x:xs) = x : a : intersperse'' a xs

flatten :: [[a]] -> [a]
flatten [] = []
flatten (xs:xxs) = xs ++ flatten xxs

intercalate'' :: [a] -> [[a]] -> [a]
intercalate'' a b = flatten $ intersperse'' a b

concat'' = flatten

concatMap'' f xs = flatten $ map f xs

and'' :: [Bool] -> Bool
and'' [] = False
and'' (x:xs) = x && (and'' xs)

and2 :: [Bool] -> Bool
and2 xs = foldl (&&) True xs

iterate'' :: (a -> a) -> a -> [a]
iterate'' f a = a : iterate'' f (f a)

splitAt'' :: (Integral a) => a -> [b] -> ([b],[b])
splitAt'' _ [] = ([], [])
splitAt'' n ls@(x:xs)
    | n == 0 = ([], ls)
    | n > 0 = (x : f, t)
    | otherwise = error "Number should be >= 0"
    where f = fst $ splitAt'' (n-1) xs
          t = snd $ splitAt'' (n-1) xs

takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' _ [] = []
takeWhile'' f (x:xs)
    | f x = x : takeWhile'' f xs
    | otherwise = []

dropWhile'' :: (a -> Bool) -> [a] -> [a]
dropWhile'' _ [] = []
dropWhile'' f (x:xs)
    | f x = dropWhile'' f xs
    | otherwise = xs

span'' :: (a -> Bool) -> [a] -> ([a],[a])
span'' _ [] = ([], [])
span'' p ls@(x:xs)
    | p x = (x:f, t)
    | otherwise = ([], ls)
    where f = fst $ span'' p xs
          t = snd $ span'' p xs

break'' :: (a -> Bool) -> [a] -> ([a], [a])
break'' _ [] = ([], [])
break'' p ls@(x:xs)
    | p x = ([], ls)
    | otherwise = (x:f, t)
    where f = fst $ break'' p xs
          t = snd $ break'' p xs

group'' :: (Eq a) => [a] -> [[a]]
group'' [] = [[]]
group'' [x] = [[x]]
group'' (x:xs)
    | x == head xs = (x:x1) : tail (group'' xs)
    | otherwise = [x] : group'' xs
    where x1:xxs = group'' xs

groupBetter :: (Eq a) => [a] -> [[a]]
groupBetter [] = []
groupBetter (x:xs) = (x:f) : groupBetter t
    where (f,t) = span (==x) xs
    
partition'' :: (a -> Bool) -> [a] -> ([a],[a])
partition'' _ [] = ([],[])
partition'' p (x:xs)
    | p x = (x:f, t)
    | otherwise = (f, x:t)
    where f = fst $ partition'' p xs
          t = snd $ partition'' p xs

unlines'' :: [String] -> String
unlines'' = intercalate'' "\n"

split'' :: (a -> Bool) -> [a] -> [[a]]
split'' _ [] = []
split'' p x = case break p x of
                (f, n:next) -> f : (split'' p next)
                (f, []) -> [f]

lines'' :: String -> [String]
lines'' = split'' (=='\n')

unwords'' :: [String] -> String
unwords'' = intercalate'' " "

words'' :: String -> [String]
words'' = split'' (==' ')
