module Eval
( eval
) where

import Data.IORef
import qualified Data.Map.Strict as Map

import Types
import Parser

type Env = IORef (Map.Map String S_Object)
type Continuation = S_Object -> IO ()

eval :: Env -> Continuation -> S_Program -> IO ()
