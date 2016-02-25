import System.Random
import Control.Monad

main = do
    gen <- getStdGen
    askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
    let (num, newGen) = randomR (1,10) gen :: (Int, StdGen)
    putStrLn "Which number from 1 to 10 am I thinking of? "
    numStr <- getLine
    
    when (not.null $ numStr) $ do
        let guess = read numStr
        if guess == num
            then putStrLn "You are correct!"
            else putStrLn $ "Sorry, it was " ++ show num
        askForNumber newGen