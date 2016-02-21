import Control.Monad
import Data.Char

{-main = do
    colors <- forM [1,2,3,4] (\a -> do
        putStrLn $ "Which color do you associate with the number " ++ (show a) ++ "?"
        color <- getLine
        return color)
    putStrLn "The colors that you associate with the numbers 1, 2, 3, 4 are:"
    mapM putStrLn colors
-}
main = forever $ do
    putStr "Give me some input: "
    line <- getLine
    putStrLn $ map toUpper line