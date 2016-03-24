import Data.List
import Control.Monad

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "*" = return ((x * y):ys)
foldingFunction (x:y:ys) "/" = return ((y/x):ys)
foldingFunction (x:y:ys) "-" = return ((y-x):ys)
foldingFunction (x:y:ys) "+" = return ((x+y):ys)
foldingFunction xs numStr = liftM (:xs) (readMaybe numStr)

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of
    [(x,"")] -> Just x
    _ -> Nothing

solve :: String -> Maybe Double
solve str = do
    [result] <- foldM foldingFunction [] (words str)
    return result
