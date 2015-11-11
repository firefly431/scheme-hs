module Eval
( eval
, callFunction
, interpret
) where

import Data.IORef
import qualified Data.Map.Strict as Map

import System.IO

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Trans.Class

import Types
import Parser
import Preprocess

callFunction :: S_Object -> S_Object -> SCont
callFunction (C_Builtin f) a = runBuiltin f $ a
callFunction (C_Lambda (L_Closure env) params (L_Program prog)) args = (lift $ injectParams env params args) >>= prog
callFunction a b = lift $ throwError (NotFunction . display $ a)

lookupVariable :: Env -> String -> ExceptT S_Error IO (IORef S_Object)
lookupVariable env var = do
        env' <- lift $ readIORef env
        case Map.lookup var env' of
            Just a -> return a
            Nothing -> throwError (UndefinedVariable var)

defineVariable :: Env -> String -> ExceptT S_Error IO ()
defineVariable env var = do
    io <- lift $ newIORef undefinedObject
    lift $ modifyIORef' env (Map.insert var io)

assignVariable :: Env -> String -> S_Object -> ExceptT S_Error IO ()
assignVariable env var val = do
    varref <- lookupVariable env var
    lift $ writeIORef varref val

createClosure :: Env -> S_Object -> ExceptT S_Error IO Env
createClosure env params = do
    newenv <- lift $ (readIORef env >>= newIORef)
    variables params >>= mapM_ (defineVariable newenv)
    return newenv
    where
        variables :: S_Object -> ExceptT S_Error IO [String]
        variables (C_List C_EmptyList) = return []
        variables (C_List (C_Cons a b)) = do
            a' <- variable a
            b' <- variables b
            return $ a' : b'
        variables a@(C_Symbol _) = fmap return $ variable a
        variables a = throwError (Syntax ("invalid parameter list " ++ (display a)))
        variable :: S_Object -> ExceptT S_Error IO String
        variable (C_Symbol a) = return a
        variable a = throwError (Syntax ("invalid parameter " ++ (display a)))

injectParams :: Env -> S_Object -> S_Object -> ExceptT S_Error IO Env
injectParams env params args = do
    newenv <- lift $ (readIORef env >>= newIORef)
    zipArgs newenv params args
    return newenv
    where
        zipArgs :: Env -> S_Object -> S_Object -> ExceptT S_Error IO ()
        zipArgs env (C_List C_EmptyList) (C_List C_EmptyList) = return ()
        zipArgs env (C_List (C_Cons (C_Symbol px) pxs)) (C_List (C_Cons ax axs)) = assignVariable env px ax >> zipArgs env pxs axs
        zipArgs env (C_List (C_Cons _ _)) (C_List C_EmptyList) = throwError (ArgumentError "not enough arguments")
        zipArgs env (C_List C_EmptyList) (C_List (C_Cons _ _)) = throwError (ArgumentError "too many arguments")
        zipArgs env _ _ = throwError (ArgumentError "invalid argument")

eval :: Env -> S_Program -> SCont
eval _ (P_Literal a) = return a
eval env (P_Lookup n) = (lift $ lookupVariable env n) >>= (fmap (lift . lift) readIORef)
eval env (P_Call f a) = do
    f' <- eval env f
    a' <- eval env a
    callFunction f' a'
eval env (P_Sequence a b) = (eval env a) >> (eval env b)
eval _ (P_BuildEmptyList) = return $ C_List C_EmptyList
eval env (P_BuildList a b) = do
    a' <- eval env a
    b' <- eval env b
    return $ C_List $ C_Cons a' b'
eval env (P_Assignment var val) = eval env val >>= (fmap lift $ assignVariable env var) >> return undefinedObject
eval env (P_Definition var val) = lift (defineVariable env var) >> eval env val >>= (fmap lift $ assignVariable env var) >> return undefinedObject
eval env (P_Conditional cond t f) = do
    cond' <- eval env cond
    case cond' of
        C_Bool False -> eval env f -- only #f is false
        _ -> eval env t
eval env (P_Procedure params prog) = do
    closure <- lift $ createClosure env params
    return (C_Lambda (L_Closure closure) params (L_Program $ flip eval prog))
eval _ (P_Undefined) = return undefinedObject

interpret :: Env -> String -> SCont
interpret e = eval e . preprocess_body base_context . parse
