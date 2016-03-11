import Control.Monad.Writer

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

newtype DiffList a = DiffList {getDiffList :: [a] -> [a]}

instance Monoid (DiffList a) where
    mempty = toDiffList []
    mappend (DiffList f) (DiffList g) = DiffList (\xs -> f (g xs))

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++ )

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

countdownDiff :: Int -> Writer (DiffList String) ()
countdownDiff 0 = do
    tell (toDiffList ["0"])
countdownDiff x = do
    countdownDiff (x-1)
    tell (toDiffList [show x])

countdown :: Int -> Writer [String] ()
countdown 0 = do
    tell ["0"]
countdown x = do
    countdown (x-1)
    tell [show x]

diffTest :: Int -> IO ()
diffTest = mapM_ putStrLn . fromDiffList . snd . runWriter . countdownDiff

test :: Int -> IO ()
test = mapM_ putStrLn . snd . runWriter . countdown