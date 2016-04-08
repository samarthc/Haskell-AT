module SchemeEval where

import LispVal
import GHC.Real
import Data.Complex

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinOp (+)),
              ("-", numericBinOp (-)),
              ("*", numericBinOp (*)),
              ("/", numericBinOp (*)),
              ("mod", numericBinOp mod),
              ("quotient", numericBinOp quot),
              ("remainder", numericBinOp rem),
              ("symbol?", predicate symbolp),
              ("string?", predicate stringp),
              ("bool?", predicate boolp),
              ("list?", predicate listp),
              ("pair?", predicate pairp),
              ("vector?", predicate vectorp),
              ("number?", predicate numberp),
              ("complex?", predicate complexp),
              ("real?", predicate realp),
              ("rational?", predicate rationalp),
              ("integer?", predicate integerp),
              ("symbol->string", conversion sym2str),
              ("string->symbol", conversion str2sym)]

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinOp op params = Number $ foldl1 op $ map unpackNum params

predicate :: (LispVal -> Bool) -> [LispVal] -> LispVal
predicate pred [val] = Bool $ pred val

conversion :: (LispVal -> LispVal) -> [LispVal] -> LispVal
conversion conv [va] = conv val

unpackNum :: Num a => LispVal -> a
unpackNum (Number num) = fromIntegral num
unpackNum _ = 0
--unpackNum (Float num) = num
--unpackNum (Ratio num) = fromRational num
--unpackNum (Complex num) = num

symbolp, stringp, boolp, listp, pairp, vectorp, numberp, complexp, realp, rationalp, integerp :: LispVal -> Bool

symbolp (Atom _) = True
symbolp _ = False

stringp (String _) = True
stringp _ = False

boolp (Bool _) = True
boolp _ = False

listp (List _) = True
listp _ = False

pairp (List []) = False
pairp (List _) = True
pairp (DottedList _ _) = True
pairp _ = False

vectorp (Vector _) = True
vectorp _ = False

numberp (Number _) = True
numberp (Complex _) = True
numberp (Float _) = True
numberp (Ratio _) = True
numberp _ = False

complexp = numberp

realp (Complex (_ :+ 0)) = True
realp arg = numberp arg

rationalp = realp

integerp (Number _) = True
integerp (Complex (a :+ 0)) = isInt a
integerp (Float a) = isInt a
integerp (Ratio (_ :% 1)) = True
integerp _ = False

isInt :: Double -> Bool
isInt a = frac a == 0

frac :: Double -> Double
frac x
    | x < 0 = frac (negate x)
    | (x-1) < 0 = x
    | otherwise = frac (x-1)

sym2str, str2sym :: LispVal -> LispVal
sym2str (Atom s) = String s
str2sym (String s) = Atom s

eval :: LispVal -> LispVal
eval val@(Number _) = val
eval val@(Float _) = val
eval val@(Ratio _) = val
eval val@(Complex _) = val
eval val@(Character _) = val
eval val@(String _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args
eval (List []) = List []

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives
