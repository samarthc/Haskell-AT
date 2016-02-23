import Data.Char
import Control.Monad

main = forever $ do
    line <- getLine
    let caps = map toUpper line
    putStrLn line
