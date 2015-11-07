module Builtins
( builtins
) where

import Types

import Control.Monad
import Control.Monad.Except
import Data.Complex
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

instance Convertable B_Number where
    convert (C_Number x) = return . BoxN $ x
    convert _ = throwError $ AssertionViolation "expected number"
    unconvert = C_Number . unbox

folds :: (Convertable a) => (a -> a -> a) -> a -> S_Object -> ExceptT S_Error IO a
folds f a (C_List C_EmptyList) = return a
folds f a (C_List (C_Cons x xs)) = convert x >>= return . f a >>= (\a' -> seq a' $ folds f a' xs)
folds f a _ = throwError (ArgumentError "invalid argument")

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

builtins :: [(String, BuiltinFunction)]
builtins =
    [ ("+", BuiltinFunction $ fmap unconvert . (folds (+) (BoxN $ 0 :+ 0)))
    , ("-", BuiltinFunction $ \a -> case a of
        (C_List (C_Cons x xs)) -> case xs of
            (C_List C_EmptyList) -> fmap (unconvert . negate) $ (convert x :: ExceptT S_Error IO B_Number)
            _ -> convert x >>= \x' -> fmap unconvert . folds (-) (x' :: B_Number) $ xs
        _ -> throwError (ArgumentError "invalid argument")
        )
    , ("*", BuiltinFunction $ fmap unconvert . (folds (*) (BoxN $ 1 :+ 0)))
    , ("/", BuiltinFunction $ \a -> case a of
        (C_List (C_Cons x xs)) -> case xs of
            (C_List C_EmptyList) -> fmap (unconvert . recip) $ (convert x :: ExceptT S_Error IO B_Number)
            _ -> convert x >>= \x' -> fmap unconvert . folds (/) (x' :: B_Number) $ xs
        _ -> throwError (ArgumentError "invalid argument")
        )
    , ("write", BuiltinFunction $ (>>= (>> return undefinedObject) . lift . putStr . show) . extractSingleton)
    , ("display", BuiltinFunction $ (>>= (>> return undefinedObject) . lift . putStr . display) . extractSingleton)
    , ("newline", BuiltinFunction $ (>> return undefinedObject) . lift . putChar . const '\n')
    , ("exit", BuiltinFunction $ lift . exitWith . (\x -> case x of C_Bool False -> ExitFailure 1; _ -> ExitSuccess))
    , ("error", BuiltinFunction $ throwError . User . processError)
    ]
