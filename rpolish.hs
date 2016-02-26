import MyList (split'')
import Data.List (lookup)

--funcs :: [(String, (Num a => a -> a -> a))]
--funcs = [("+",(+)), ("-", (-)), ("*", (*)), ("/", (/)), ("^",(^))]

eval (x:y:rest) "+" = show (read y + read x) : rest
eval (x:y:rest) "-" = show (read y - read x) : rest
eval (x:y:rest) "*" = show (read y * read x) : rest
eval (x:y:rest) "/" = show (read y / read x) : rest
eval acc next = next : acc

evaluate :: String -> [String]
evaluate exp = foldl eval [] . split'' (==' ') $ exp

result :: String -> Int
result =  read . sing . evaluate
    where sing [x] = x