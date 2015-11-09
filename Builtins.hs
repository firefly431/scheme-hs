module Builtins
( builtins
) where

import Types

import Control.Monad
import Control.Monad.Except
import Data.Complex
import Data.List
import Data.Function
import System.Exit

class Convertable a where
    convert :: S_Object -> ExceptT S_Error IO a
    unconvert :: a -> S_Object

newtype B_Number = BoxN { unbox :: I_Number }

instance Num B_Number where
    (+) = curry $ BoxN . uncurry ((+) `on` unbox)
    (*) = curry $ BoxN . uncurry ((*) `on` unbox)
    (-) = curry $ BoxN . uncurry ((-) `on` unbox)
    negate = BoxN . negate . unbox
    fromInteger = BoxN . fromInteger
    signum = BoxN . signum . unbox
    abs = BoxN . abs . unbox

instance Fractional B_Number where
    (/) = curry $ BoxN . uncurry ((/) `on` unbox)
    recip = BoxN . recip . unbox
    fromRational = BoxN . fromRational

instance Eq B_Number where
    (==) = (==) `on` realPart . unbox
    (/=) = (/=) `on` unbox

instance Ord B_Number where
    compare = compare `on` (realPart . unbox)

instance Convertable B_Number where
    convert (C_Number x) = return . BoxN $ x
    convert _ = throwError $ AssertionViolation "expected number"
    unconvert = C_Number . unbox

asList :: (Convertable a) => S_Object -> ExceptT S_Error IO [a]
asList (C_List C_EmptyList) = return []
asList (C_List (C_Cons x xs)) = do
    x' <- convert x
    xs' <- asList xs
    return $ x':xs'
asList _ = throwError (ArgumentError "invalid argument")

number :: b -> (a -> b) -> ([a] -> b) -> [a] -> b
number zero one many list = case list of
    [] -> zero
    [a] -> one a
    _ -> many list

processIrritants :: String -> S_Object -> String
processIrritants m (C_List C_EmptyList) = m
processIrritants m (C_List (C_Cons x (C_List C_EmptyList))) = m ++ ": " ++ (display x)
processIrritants m x = m ++ ": " ++ (display x)

processError :: S_Object -> String
processError (C_List C_EmptyList) = "user error"
processError (C_List (C_Cons (C_String s) xs)) = processIrritants s xs
processError (C_List (C_Cons (C_Symbol s) xs@(C_List (C_Cons (C_String _) _)))) = "in " ++ s ++ ": " ++ processError xs
processError (C_List (C_Cons (C_Bool False) xs@(C_List (C_Cons (C_String _) _)))) = processError xs
processError xs@(C_List _) = processIrritants "user error" xs
processError _ = "user error"

extractSingleton :: S_Object -> ExceptT S_Error IO S_Object
extractSingleton (C_List (C_Cons x (C_List C_EmptyList))) = return x
extractSingleton _ = throwError (ArgumentError "invalid argument or too many/few arguments")

foldcf :: (Convertable a) => (a -> a -> a) -> a -> S_Object -> ExceptT S_Error IO S_Object
foldcf f d = fmap unconvert . (\l -> fmap (foldl' f d) (asList l))

foldcnf :: (Convertable a) => a -> (a -> a) -> (a -> a -> a) -> S_Object -> ExceptT S_Error IO S_Object
foldcnf z o m = fmap unconvert . (\l -> fmap (number z o (foldl1' m)) (asList l))

comparef :: (Convertable a, Ord a) => (a -> a -> Bool) -> Bool -> a -> S_Object -> ExceptT S_Error IO S_Object
comparef f z o = fmap C_Bool . (\l -> fmap (number z (\a -> f a o) (\l -> all (uncurry f) $ zip l (tail l))) (asList l))

builtins :: [(String, BuiltinFunction)]
builtins = map (fmap BuiltinFunction)
    [ ("+", foldcf (+) (BoxN $ 0 :+ 0))
    , ("-", foldcnf (BoxN $ 0 :+ 0) negate (-))
    , ("*", foldcf (*) (BoxN $ 1 :+ 0))
    , ("/", foldcnf (BoxN $ 1 :+ 0) recip (/))
    , ("<", comparef (<) False (BoxN $ 0 :+ 0))
    , (">", comparef (>) False (BoxN $ 0 :+ 0))
    , ("<=", comparef (<=) True (BoxN $ 0 :+ 0))
    , (">=", comparef (>=) True (BoxN $ 0 :+ 0))
    , ("write", (>>= (>> return undefinedObject) . lift . putStr . show) . extractSingleton)
    , ("display", (>>= (>> return undefinedObject) . lift . putStr . display) . extractSingleton)
    , ("newline", (>> return undefinedObject) . lift . putChar . const '\n')
    , ("exit", lift . exitWith . (\x -> case x of C_Bool False -> ExitFailure 1; _ -> ExitSuccess))
    , ("error", throwError . User . processError)
    ]