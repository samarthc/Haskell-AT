module SchemeEval where

import LispVal
import LispError
import Control.Monad.Error
import GHC.Real
import Data.Complex

primitives :: [(String, [LispVal] -> Either LispError LispVal)]
primitives = [("+", numericBinOp (+)),
              ("-", numericBinOp (-)),
              ("*", numericBinOp (*)),
              ("/", numericBinOp div),
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
              ("string->symbol", conversion str2sym),
              ("=", numCompare (==)),
              ("<", numCompare (<)),
              (">", numCompare (>)),
              ("/=", numCompare (/=)),
              (">=", numCompare (>=)),
              ("<=", numCompare (<=)),
              ("&&", boolCombine (&&)),
              ("||", boolCombine (||)),
              ("string=?", strCompare (==)),
              ("string<?", strCompare (<)),
              ("string>?", strCompare (>)),
              ("string<=?", strCompare (<=)),
              ("string>=?", strCompare (>=))]

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> Either LispError LispVal
numericBinOp _ [] = throwError $ NumArgs 2 []
numericBinOp _ val@[_] = throwError $ NumArgs 2 val
numericBinOp op params = mapM unpackNum params >>= return . Number . foldl1 op

predicate :: (LispVal -> Bool) -> [LispVal] -> Either LispError LispVal
predicate pred [val] = return . Bool $ pred val
predicate pred args = throwError $ NumArgs 1 args

conversion :: (LispVal -> Either LispError LispVal) -> [LispVal] -> Either LispError LispVal
conversion conv [val] = conv val
conversion _ args = throwError $ NumArgs 1 args

numCompare = boolBinOp unpackNum
boolCombine = boolBinOp unpackBool
strCompare = boolBinOp unpackStr

boolBinOp :: (LispVal -> Either LispError a) -> (a -> a -> Bool) -> [LispVal] -> Either LispError LispVal
boolBinOp unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- (unpacker . head $ args)
                                     right <- (unpacker . head . tail $ args)
                                     return . Bool $ left `op` right

unpackNum :: Num a => LispVal -> Either LispError a
unpackNum (Number num) = return $ fromIntegral num
unpackNum notNum = throwError $ TypeMismatch "number" notNum
--unpackNum (Float num) = num
--unpackNum (Ratio num) = fromRational num
--unpackNum (Complex num) = num

unpackBool :: LispVal -> Either LispError Bool
unpackBool (Bool bool) = return bool
unpackBool notBool = throwError $ TypeMismatch "bool" notBool

unpackStr :: LispVal -> Either LispError String
unpackStr (String str) = return str
unpackStr notStr = throwError $ TypeMismatch "string" notStr

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

realp (Complex num) = imagPart num == 0
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

sym2str, str2sym :: LispVal -> Either LispError LispVal
sym2str (Atom s) = return $ String s
sym2str arg = throwError $ TypeMismatch "symbol" arg
str2sym (String s) = return $ Atom s
str2sym arg = throwError $ TypeMismatch "string" arg

eval :: LispVal -> Either LispError LispVal
eval val@(Number _) = return val
eval val@(Float _) = return val
eval val@(Ratio _) = return val
eval val@(Complex _) = return val
eval val@(Character _) = return val
eval val@(String _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval (List []) = return $ List []
eval badform = throwError $ BadSpecialForm "Unrecognized form" badform

apply :: String -> [LispVal] -> Either LispError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function" func) ($ args) $ lookup func primitives
