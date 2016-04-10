module SchemeEval where

import LispVal
import LispError
import Control.Monad.Error
import GHC.Real
import Data.Complex
import Data.Char (toLower)
import Data.List (genericLength, genericDrop, genericTake, genericReplicate, genericIndex)
import SchemeEnv

eval :: Env -> LispVal -> ErrorT LispError IO LispVal
eval env val@(Number _) = return val
eval env val@(Float _) = return val
eval env val@(Ratio _) = return val
eval env val@(Complex _) = return val
eval env val@(Character _) = return val
eval env val@(String _) = return val
eval env val@(Bool _) = return val
eval env (Atom var) = getVar env var
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq]) = do
    result <- eval env pred
    case result of
        Bool False -> return Unspecified
        otherwise -> eval env conseq
eval env (List [Atom "if", pred, conseq, alt]) = do
    result <- eval env pred
    case result of
        Bool False -> eval env alt
        otherwise -> eval env conseq

eval env form@(List (Atom "case" : key : clauses)) = 
    if null clauses
    then return Unspecified
    else case head clauses of
        List (Atom "else" : exprs) -> mapM (eval env) exprs >>= return . last
        List ((List datums) : exprs) -> do
            result <- eval env key
            equality <- mapM (\x -> eqv [result, x]) datums
            if Bool True `elem` equality
                then mapM (eval env) exprs >>= return . last
                else eval env $ List (Atom "case" : key : tail clauses)
        otherwise -> throwError $ BadSpecialForm "ill-formed case expression" form

eval env form@(List (Atom "cond" : clauses)) = 
    if null clauses
    then return Unspecified
    else case head clauses of
        List (Atom "else" : exprs) -> if (null . tail $ clauses)
                                      then mapM (eval env) exprs >>= return . last
                                      else throwError $ BadSpecialForm "misplaced else clause" (List (Atom "else" : exprs))
        List (test : exprs) -> do
            result <- eval env test
            case result of
                Bool False -> eval env (List (Atom "cond" : tail clauses))
                otherwise -> mapM (eval env) exprs >>= return . last

eval env (List (Atom "and" : exprs)) = 
    if null exprs
    then return $ Bool True
    else do
        result <- eval env $ head exprs
        case result of
            Bool False -> return result
            otherwise -> if (null . tail $ exprs)
                         then return result
                         else eval env (List (Atom "and" : tail exprs))

eval env (List (Atom "or" : exprs)) =
    if null exprs
    then return $ Bool False
    else do
        result <- eval env $ head exprs
        case result of
            Bool False -> eval env (List (Atom "or" : tail exprs))
            otherwise -> return result

eval env (List [Atom "set!", Atom var, value]) = eval env value >>= setVar env var

eval env (List [Atom "define", Atom var, value]) = eval env value >>= defineVar env var
eval env (List (Atom func : args)) = mapM (eval env) args >>= apply func
eval env (List []) = return $ List []
eval env badform = throwError $ BadSpecialForm "Unrecognized form" badform

apply :: String -> [LispVal] -> ErrorT LispError IO LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function" func) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> ErrorT LispError IO LispVal)]
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
              ("string>=?", strCompare (>=)),
              ("string-ci=?", strComparei (==)),
              ("string-ci<?", strComparei (<)),
              ("string-ci>?", strComparei (>)),
              ("string-ci<=?", strComparei (<=)),
              ("string-ci>=?", strComparei (>=)),
              ("make-string", makeString),
              ("string", newString),
              ("string-length", strLen),
              ("string-ref", strRef),
              ("substring", subStr),
              ("string-append", strApp),
              ("string->list", conversion str2list),
              ("list->string", conversion list2str),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ErrorT LispError IO LispVal
numericBinOp _ [] = throwError $ NumArgs 2 []
numericBinOp _ val@[_] = throwError $ NumArgs 2 val
numericBinOp op params = mapM unpackNum params >>= return . Number . foldl1 op

predicate :: (LispVal -> Bool) -> [LispVal] -> ErrorT LispError IO LispVal
predicate pred [val] = return . Bool $ pred val
predicate pred args = throwError $ NumArgs 1 args

conversion :: (LispVal -> ErrorT LispError IO LispVal) -> [LispVal] -> ErrorT LispError IO LispVal
conversion conv [val] = conv val
conversion _ args = throwError $ NumArgs 1 args

numCompare = boolBinOp unpackNum
boolCombine = boolBinOp unpackBool
strCompare = boolBinOp unpackStr
strComparei = boolBinOp unpackStri
boolBinOp :: (LispVal -> ErrorT LispError IO a) -> (a -> a -> Bool) -> [LispVal] -> ErrorT LispError IO LispVal
boolBinOp unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- (unpacker . head $ args)
                                     right <- (unpacker . head . tail $ args)
                                     return . Bool $ left `op` right

unpackNum :: Num a => LispVal -> ErrorT LispError IO a
unpackNum (Number num) = return $ fromIntegral num
unpackNum notNum = throwError $ TypeMismatch "number" notNum
--unpackNum (Float num) = num
--unpackNum (Ratio num) = fromRational num
--unpackNum (Complex num) = num

unpackBool :: LispVal -> ErrorT LispError IO Bool
unpackBool (Bool bool) = return bool
unpackBool notBool = throwError $ TypeMismatch "bool" notBool

unpackStr :: LispVal -> ErrorT LispError IO String
unpackStr (String str) = return str
unpackStr notStr = throwError $ TypeMismatch "string" notStr

unpackStri :: LispVal -> ErrorT LispError IO String
unpackStri (String str) = return . map toLower $ str
unpackStri notStr = throwError $ TypeMismatch "string" notStr

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

sym2str, str2sym, str2list, list2str :: LispVal -> ErrorT LispError IO LispVal
sym2str (Atom s) = return $ String s
sym2str badArg = throwError $ TypeMismatch "symbol" badArg

str2sym (String s) = return $ Atom s
str2sym badArg = throwError $ TypeMismatch "string" badArg

str2list (String str) = return . List . map Character $ str
str2list badArg = throwError $ TypeMismatch "string" badArg

list2str (List []) = return $ String ""
list2str (List ((Character ch):xs)) = list2str (List xs) >>= unpackStr >>= return . String . (ch:)
list2str (List (badArg:_)) = throwError $ TypeMismatch "character" badArg
list2str badArg = throwError $ TypeMismatch "list" badArg

makeString :: [LispVal] -> ErrorT LispError IO LispVal
makeString [] = throwError $ NumArgs 2 []
makeString [(Number len)] = return . String $ genericReplicate len '\NUL'
makeString [badArg] = throwError $ TypeMismatch "integer" badArg
makeString [(Number len), (Character ch)] = return . String $ genericReplicate len ch
makeString [(Number len), badArg] = throwError $ TypeMismatch "character" badArg
makeString [badArg, _] = throwError $ TypeMismatch "integer" badArg
makeString badArgList = throwError $ NumArgs 2 badArgList

newString :: [LispVal] -> ErrorT LispError IO LispVal
newString [] = return . String $ ""
newString ((Character ch):xs) = newString xs >>= unpackStr >>= return . String . (ch:)
newString (badArg:_) = throwError $ TypeMismatch "character" badArg

strLen :: [LispVal] -> ErrorT LispError IO LispVal
strLen [(String str)] = return . Number . genericLength $ str
strLen [badArg] = throwError $ TypeMismatch "string" badArg
strLen badArgList = throwError $ NumArgs 1 badArgList

strRef :: [LispVal] -> ErrorT LispError IO LispVal
strRef [(String str), (Number i)]
    | i>=0 && i < genericLength str = return . Character $ str `genericIndex` i
    | otherwise = throwError $ Default "index out of range"
strRef [badArg, (Number _)] = throwError $ TypeMismatch "string" badArg
strRef [(String str), badArg] = throwError $ TypeMismatch "integer" badArg
strRef badArgList = throwError $ NumArgs 2 badArgList

subStr :: [LispVal] -> ErrorT LispError IO LispVal
subStr [(String str), (Number start), (Number end)]
    | start < 0 || start > genericLength str = throwError $ Default "first index out of range"
    | end < 0 || end > genericLength str = throwError $ Default "second index out of range"
    | start > end = throwError $ Default "first index cannot be greater than second index"
    | otherwise = return . String . genericTake (end - start) . genericDrop start $ str
subStr [badArg, (Number _), (Number _)] = throwError $ TypeMismatch "string" badArg
subStr [_, badArg, (Number _)] = throwError $ TypeMismatch "integer" badArg
subStr [_, _, badArg] = throwError $ TypeMismatch "integer" badArg
subStr badArgList = throwError $ NumArgs 3 badArgList

strApp :: [LispVal] -> ErrorT LispError IO LispVal
strApp [] = return . String $ ""
strApp ((String str):xs) = strApp xs >>= unpackStr >>= return . String . (str++)
strApp (badArg:_) = throwError $ TypeMismatch "string" badArg

car :: [LispVal] -> ErrorT LispError IO LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ErrorT LispError IO LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ErrorT LispError IO LispVal
cons [x, List xs] = return $ List (x:xs)
cons [x, DottedList xs last] = return $ DottedList (x:xs) last
cons [x, y] = return $ DottedList [x] y
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ErrorT LispError IO LispVal
eqv [(Bool arg1), (Bool arg2)] = return . Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return . Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return . Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return . Bool $ arg1 == arg2
eqv [(List []), (List [])] = return . Bool $ True
eqv [_, _] = return . Bool $ False
eqv badArgList = throwError $ NumArgs 2 badArgList

equal :: [LispVal] -> ErrorT LispError IO LispVal
equal val@[(Bool _), (Bool _)] = eqv val
equal val@[(Number _), (Number _)] = eqv val
equal val@[(String _), (String _)] = eqv val
equal val@[(Atom _), (Atom _)] = eqv val
equal [(DottedList xs x), (DottedList ys y)] = equal [List (xs ++ [x]), List (ys ++ [y])]
equal [(List []), (List [])] = return . Bool $ True
equal [(List (x:xs)), (List [])] = return . Bool $ False
equal [(List []), (List (y:ys))] = return . Bool $ False
equal [(List (x:xs)), (List (y:ys))] = if x==y
    then equal [List xs, List ys]
    else return . Bool $ False
equal [_, _] = return . Bool $ False
equal badArgList = throwError $ NumArgs 2 badArgList
