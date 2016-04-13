{-# LANGUAGE ScopedTypeVariables #-}
module SchemeInit where

import SchemeEnv
import SchemeParsers
import LispVal
import GHC.Real
import Control.Monad.Error
import Data.Char (toLower)
import Data.Complex
import Data.IORef
import Data.List (genericTake, genericLength, genericDrop, genericIndex, genericReplicate)
import System.IO
import System.IO.Unsafe
import Control.Exception

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc PrimitiveFunc) primitives ++ map (makeFunc IOFunc) ioPrimitives)
    where
        makeFunc constructor (var, func) = (var, constructor func)

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
strCompare = boolBinOp unpackStr
strComparei = boolBinOp unpackStri
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

unpackStri :: LispVal -> Either LispError String
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

sym2str, str2sym, str2list, list2str :: LispVal -> Either LispError LispVal
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

makeString :: [LispVal] -> Either LispError LispVal
makeString [] = throwError $ NumArgs 2 []
makeString [(Number len)] = return . String $ genericReplicate len '\NUL'
makeString [badArg] = throwError $ TypeMismatch "integer" badArg
makeString [(Number len), (Character ch)] = return . String $ genericReplicate len ch
makeString [(Number len), badArg] = throwError $ TypeMismatch "character" badArg
makeString [badArg, _] = throwError $ TypeMismatch "integer" badArg
makeString badArgList = throwError $ NumArgs 2 badArgList

newString :: [LispVal] -> Either LispError LispVal
newString [] = return . String $ ""
newString ((Character ch):xs) = newString xs >>= unpackStr >>= return . String . (ch:)
newString (badArg:_) = throwError $ TypeMismatch "character" badArg

strLen :: [LispVal] -> Either LispError LispVal
strLen [(String str)] = return . Number . genericLength $ str
strLen [badArg] = throwError $ TypeMismatch "string" badArg
strLen badArgList = throwError $ NumArgs 1 badArgList

strRef :: [LispVal] -> Either LispError LispVal
strRef [(String str), (Number i)]
    | i>=0 && i < genericLength str = return . Character $ str `genericIndex` i
    | otherwise = throwError $ Default "index out of range"
strRef [badArg, (Number _)] = throwError $ TypeMismatch "string" badArg
strRef [(String str), badArg] = throwError $ TypeMismatch "integer" badArg
strRef badArgList = throwError $ NumArgs 2 badArgList

subStr :: [LispVal] -> Either LispError LispVal
subStr [(String str), (Number start), (Number end)]
    | start < 0 || start > genericLength str = throwError $ Default "first index out of range"
    | end < 0 || end > genericLength str = throwError $ Default "second index out of range"
    | start > end = throwError $ Default "first index cannot be greater than second index"
    | otherwise = return . String . genericTake (end - start) . genericDrop start $ str
subStr [badArg, (Number _), (Number _)] = throwError $ TypeMismatch "string" badArg
subStr [_, badArg, (Number _)] = throwError $ TypeMismatch "integer" badArg
subStr [_, _, badArg] = throwError $ TypeMismatch "integer" badArg
subStr badArgList = throwError $ NumArgs 3 badArgList

strApp :: [LispVal] -> Either LispError LispVal
strApp [] = return . String $ ""
strApp ((String str):xs) = strApp xs >>= unpackStr >>= return . String . (str++)
strApp (badArg:_) = throwError $ TypeMismatch "string" badArg

car :: [LispVal] -> Either LispError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> Either LispError LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> Either LispError LispVal
cons [x, List xs] = return $ List (x:xs)
cons [x, DottedList xs last] = return $ DottedList (x:xs) last
cons [x, y] = return $ DottedList [x] y
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> Either LispError LispVal
eqv [(Bool arg1), (Bool arg2)] = return . Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return . Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return . Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return . Bool $ arg1 == arg2
eqv [(List []), (List [])] = return . Bool $ True
eqv [_, _] = return . Bool $ False
eqv badArgList = throwError $ NumArgs 2 badArgList

equal :: [LispVal] -> Either LispError LispVal
equal val@[(Bool _), (Bool _)] = eqv val
equal val@[(Number _), (Number _)] = eqv val
equal val@[(String _), (String _)] = eqv val
equal val@[(Atom _), (Atom _)] = eqv val
equal [(DottedList xs x), (DottedList ys y)] = equal [List (xs ++ [x]), List (ys ++ [y])]
equal [(List []), (List [])] = return . Bool $ True
equal [(List (x:xs)), (List [])] = return . Bool $ False
equal [(List []), (List (y:ys))] = return . Bool $ False
equal [(List (x:xs)), (List (y:ys))] = do
    (Bool bool) <- eqv [x, y]
    if bool
    then equal [List xs, List ys]
    else return . Bool $ False
equal [_, _] = return . Bool $ False
equal badArgList = throwError $ NumArgs 2 badArgList

ioPrimitives :: [(String, [LispVal] -> ErrorT LispError IO LispVal)]
ioPrimitives = [("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

makePort :: IOMode -> [LispVal] -> ErrorT LispError IO LispVal
makePort mode [String filename] = fmap Port . liftIO $ openFile filename mode
makePort _ [badArg] = throwError $ TypeMismatch "string" badArg
makePort _ badArgList = throwError $ NumArgs 1 badArgList

closePort :: [LispVal] -> ErrorT LispError IO LispVal
closePort [Port port] = liftIO $ hClose port >> (return Unspecified)
closePort [badArg] = throwError $ TypeMismatch "port" badArg
closePort badArgList = throwError $ NumArgs 1 badArgList

readProc :: [LispVal] -> ErrorT LispError IO LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= (liftThrows . readExpr)
readProc [badArg] = throwError $ TypeMismatch "port" badArg
readProc badArgList = throwError $ NumArgs 1 badArgList

writeProc :: [LispVal] -> ErrorT LispError IO LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> return Unspecified

readContents :: [LispVal] -> ErrorT LispError IO LispVal
readContents [String filename] = fmap String . liftIO $ readFile filename
readContents [badArg] = throwError $ TypeMismatch "string" badArg
readContents badArgList = throwError $ NumArgs 1 badArgList


readerr :: String -> IO (Either LispError [LispVal])
readerr filename = do 
                    result <- try (readFile filename)
	            case result of
	             (Right filecontent) -> return $ (readExprList filecontent)
	             (Left (ex :: IOException)) -> return $ Left (Default (show ex) )

load :: String -> ErrorT LispError IO [LispVal]
load filename = liftThrows (unsafePerformIO a)
         where a = do 
	            val <- (readerr filename) 
	 	    case val of
		        (Left error) -> return (Left error)
		 	(Right vals) -> return (Right vals)

readAll :: [LispVal] -> ErrorT LispError IO LispVal
readAll [String filename] = fmap List $ load filename
readAll [badArg] = throwError $ TypeMismatch "string" badArg
readAll badArgList = throwError $ NumArgs 1 badArgList

