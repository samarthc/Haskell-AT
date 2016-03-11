import Data.List

permut :: String -> [String]
permut "" = [""]
permut s = nub $ do
    let str = msort s
        n = length str - 1
    x <- [0..n]
    let (h, ht:t) = splitAt x str
        rem = h ++ t
    map (ht:) $ permut rem

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort $ odd xs) (msort $ even xs)
    where
        odd [] = []
        odd [x] = [x]
        odd (x:_:ys) = x : odd ys
        even [] = []
        even [x] = []
        even (_:y:ys) = y : even ys

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge l1@(x:xs) l2@(y:ys)
    | x <= y = x : merge xs l2
    | otherwise = y : merge l1 ys

main = do
    str <- getLine
    mapM_ putStrLn $ permut str