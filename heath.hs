import Data.List
main = do
    contents <- getContents
    let system = map fromList . groupsOf 3 . map read . lines $ contents
        path = optimalPath system
    putStrLn $ "The path with shortest travel time is (C stands for a crossover) " ++ concatMap (show . fst) path
    putStrLn $ "The travel time is " ++ (show . sum . map snd $ path)

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

data Section = Section {toA :: Int, toB :: Int, cross :: Int} deriving (Show)
type RoadSystem = [Section]

fromList :: [Int] -> Section
fromList [a,b,c] = Section a b c

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

optimalPath :: RoadSystem -> Path
optimalPath = reverse . tupMin . foldl step ([] :: Path, [] :: Path)
    where tupMin (pathA, pathB) = let priceA = sum $ map snd pathA; priceB = sum $ map snd pathB in if(priceA < priceB) then pathA else pathB

step :: (Path, Path) -> Section -> (Path, Path)
step (pathA, pathB) (Section a b c) = 
    let priceA = sum $ map snd pathA
        priceB = sum $ map snd pathB
        aPathA = priceA + a
        crossPathA = priceB + b + c
        bPathB = priceB + b 
        crossPathB = priceA + a + c
        newPathA =
            if aPathA < crossPathA
                then (A, a) : pathA
                else (C, c) : (B, b) : pathB
        newPathB = 
            if bPathB < crossPathB
                then (B, b) : pathB
                else (C, c) : (A, a) : pathA
    in (newPathA, newPathB)