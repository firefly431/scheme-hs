module Eval
( eval
) where

import Data.IORef
import qualified Data.Map.Strict as Map

import System.IO.Error
import Control.Monad.Except

import Control.Applicative

import Types
import Parser
import Preprocess

data S_Error = Default {message :: String}

instance Show S_Error where show = message

newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

type SCont = ContT () (ExceptT S_Error IO) S_Object

instance Functor m => Functor (ContT r m) where
    fmap f m = ContT $ \a -> runContT m (a . f)

instance Applicative m => Applicative (ContT r m) where
    pure x = ContT ($ x)
    -- f <*> a

instance (Applicative m, Monad m) => Monad (ContT r m) where
    return = pure
    a >>= b = ContT $ \c -> runContT a $ \d -> runContT (b d) c

instance MonadTrans (ContT r) where
    lift m = ContT (liftM return m)

callCC :: ((a -> ContT r m a) -> ContT r m a) -> ContT r m a
callCC f = ContT $ \h -> runContT (f (\a -> ContT $ \_ -> h a)) h

type Env = IORef (Map.Map String S_Object)

eval :: Env -> S_Program -> SCont
eval _ (P_Literal a) = return a
eval env (P_Lookup n) = do
    env' <- lift $ liftIO $ readIORef env
    case Map.lookup n env' of
        Just a -> return a
        Nothing -> lift $ throwError (Default "Undefined variable")
{-
eval env cont (P_Call f a) = do
    f' <- eval f
    a' <- eval a
    callFunction f' a'
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
