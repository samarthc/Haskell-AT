module SchemeEval where

import LispVal
import Control.Monad.Error
import Data.List (genericLength, genericDrop)
import SchemeEnv
import SchemeInit

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

eval env (Comment c) = do
    return (Comment c)    

eval env form@(List (Atom "case" : key : clauses)) = 
    if null clauses
    then return Unspecified
    else case head clauses of
        List (Atom "else" : exprs) -> mapM (eval env) exprs >>= return . last
        List ((List datums) : exprs) -> do
            result <- eval env key
            equality <- mapM (\x -> return . Bool $ result == x) datums
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

eval env (List (Atom "define" : List (Atom var : params) : body)) = makeNormalFunc env params body >>= defineVar env var

eval env (List (Atom "define" : (DottedList (Atom var : params) varArg) : body)) = makeVarFunc varArg env params body >>= defineVar env var

eval env (List (Atom "lambda" : List params : body)) = makeNormalFunc env params body

eval env (List (Atom "lambda" : DottedList params varArgs : body)) = makeVarFunc varArgs env params body

eval env (List (Atom "lambda" : vararg@(Atom _) : body)) = makeVarFunc vararg env [] body

eval env (List [Atom "load", String filename]) = load filename >>= fmap last . mapM (eval env)

eval env (List [Atom "apply", func, args]) = do
    function <- eval env func
    argVals <- eval env args
    apply function (unwrap argVals)
    where
        unwrap :: LispVal -> [LispVal]
        unwrap (List list) = list
        unwrap val = [val]

eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals

eval env (List []) = return $ List []
eval env badform = throwError $ BadSpecialForm "Unrecognized form" badform

apply :: LispVal -> [LispVal] -> ErrorT LispError IO LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args = func args
apply (Func params varargs body closure) args =
    if genericLength params /= genericLength args && varargs == Nothing
        then throwError $ NumArgs (genericLength params) args
        else (liftIO . bindVars closure . zip params $ args) >>= bindVarArgs varargs >>= evalBody
    where
        bindVarArgs Nothing env = return env
        bindVarArgs (Just argName) env = liftIO $ bindVars env [(argName, List remainingArgs)]
        remainingArgs = genericDrop (genericLength params) args
        evalBody env = fmap last $ mapM (eval env) body
