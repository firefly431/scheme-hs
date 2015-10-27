module Eval
( eval
) where

import Data.IORef
import qualified Data.Map.Strict as Map

import System.IO.Error

import Types
import Parser
import Preprocess

newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }
type SCont = ContT () IO S_Object
callCC :: ((a -> ContT r m a) -> ContT r m a) -> ContT r m a
callCC f = ContT $ \h -> runContT (f (\a -> ContT $ \_ -> h a)) h

type Env = IORef (Map.Map String S_Object)

eval :: Env -> S_Program -> SCont
eval _ cont (P_Literal a) = cont a
eval env cont (P_Lookup n) = do
    env' <- readIORef env
    case lookup n env' of
        Just a -> cont a
        Nothing -> userError ("Undefined variable " ++ n)
eval env cont (P_Call f a) = do
    f' <- eval f
    a' <- eval a
    callFunction f' a'
{-
data S_Program =
    P_Literal S_Object
  | P_Lookup String
  | P_Call S_Program S_Program
  | P_Procedure String S_Program
  | P_Conditional S_Program S_Program S_Program
  | P_Assignment String S_Program
  | P_Sequence S_Program S_Program
  | P_BuildEmptyList
  | P_BuildList S_Program S_Program
  | P_Undefined
    deriving (Show)
-}
