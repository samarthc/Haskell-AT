data Section = Section {toA :: Int, toB :: Int, cross :: Int} deriving (Show)
type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

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