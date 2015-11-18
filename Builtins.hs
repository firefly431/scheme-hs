module Builtins
( builtins
, baseEnv
) where

import Control.Monad
import Control.Monad.Except
import Data.Complex
import qualified Data.Map.Strict as Map
import Data.List
import Data.Function
import Data.IORef
import System.Exit

import Types
import Eval

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

instance Convertable S_Object where
    convert = return
    unconvert = id

instance Convertable a => Convertable [a] where
    convert (C_List (C_Cons a b)) = do
        a' <- convert a
        b' <- convert b
        return (a' : b')
    convert (C_List C_EmptyList) = return []
    convert _ = throwError $ AssertionViolation "expected list"
    unconvert (a:b) = C_List $ C_Cons (unconvert a) (unconvert b)
    unconvert [] = C_List C_EmptyList

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

comparef :: (Convertable a) => (a -> a -> Bool) -> Bool -> a -> S_Object -> ExceptT S_Error IO S_Object
comparef f z o = fmap C_Bool . (\l -> fmap (number z (\a -> f a o) (\l -> all (uncurry f) $ zip l (tail l))) (asList l))

prettyPrint :: S_Object -> String
prettyPrint (C_String a) = a
prettyPrint x = display x

stype :: S_Object -> String
stype (C_Number _) = "number"
stype (C_List C_EmptyList) = "null"
stype (C_List _) = "pair"
stype (C_Bool _) = "boolean"
stype (C_Char _) = "char"
stype (C_Symbol _) = "symbol"
stype (C_String _) = "string"
stype (C_Builtin _) = "procedure"
stype (C_Lambda _ _ _) = "procedure"

isType :: String -> S_Object -> Bool
isType t = (== t) . stype

isList :: S_Object -> Bool
isList (C_List _) = True
isList _ = False

traitf :: (S_Object -> Bool) -> S_Object -> ExceptT S_Error IO S_Object
traitf f = fmap (C_Bool . all f) . asList

builtins :: [(String, S_Object -> SCont)]
builtins = (map (fmap (lift .)) (
    [ ("+", foldcf (+) (BoxN $ 0 :+ 0))
    , ("-", foldcnf (BoxN $ 0 :+ 0) negate (-))
    , ("*", foldcf (*) (BoxN $ 1 :+ 0))
    , ("/", foldcnf (BoxN $ 1 :+ 0) recip (/))
    , ("<", comparef (<) False (BoxN $ 0 :+ 0))
    , (">", comparef (>) False (BoxN $ 0 :+ 0))
    , ("<=", comparef (<=) True (BoxN $ 0 :+ 0))
    , (">=", comparef (>=) True (BoxN $ 0 :+ 0))
    , ("write", (>>= (>> return undefinedObject) . lift . putStr . show) . extractSingleton)
    , ("display-scheme", (>>= (>> return undefinedObject) . lift . putStr . display) . extractSingleton)
    , ("display", (>>= (>> return undefinedObject) . lift . putStr . prettyPrint) . extractSingleton)
    , ("newline", (>> return undefinedObject) . lift . putChar . const '\n')
    , ("exit", lift . exitWith . (\x -> case x of C_Bool False -> ExitFailure 1; _ -> ExitSuccess))
    , ("error", throwError . User . processError)
    , ("max", fmap (unconvert . maximum) . (asList :: S_Object -> ExceptT S_Error IO [B_Number]))
    , ("min", fmap (unconvert . minimum) . (asList :: S_Object -> ExceptT S_Error IO [B_Number]))
    , ("equal?", comparef (==) True (C_Bool False))
    , ("not", (fmap $ \x -> case x of C_Bool False -> C_Bool True; _ -> C_Bool False) . extractSingleton)
    , ("car", (>>= \x -> case x of C_List x -> (case x of C_Cons a b -> return a; _ -> throwError $ ArgumentError "empty list"); _ -> throwError $ ArgumentError "not a list") . extractSingleton)
    , ("cdr", (>>= \x -> case x of C_List x -> (case x of C_Cons a b -> return b; _ -> throwError $ ArgumentError "empty list"); _ -> throwError $ ArgumentError "not a list") . extractSingleton)
    , ("cons", \x -> case x of C_List (C_Cons a (C_List (C_Cons b (C_List C_EmptyList)))) -> return $ C_List (C_Cons a b); _ -> throwError $ ArgumentError "wrong number of arguments")
    , ("list", return)
    , ("length", (>>= fmap (C_Number . (:+ 0) . fromIntegral . length) . (asList :: S_Object -> ExceptT S_Error IO [S_Object])) . extractSingleton)
    , ("append", foldcf (++) ([] :: [S_Object]))
    , ("reverse", (fmap $ unconvert . reverse) . (extractSingleton >=> (convert :: S_Object -> ExceptT S_Error IO [S_Object])))
    , ("type%", fmap (C_String . stype) . extractSingleton)
    ] ++
    map (fmap traitf) (
        [ ("list?", isList)
        ] ++
        map (\t -> (t ++ "?", isType t)) ["number", "null", "pair", "boolean", "char", "symbol", "string", "procedure"])) ++
    [ ("call-with-current-continuation", lift . extractSingleton >=> \y -> callCC $ \x -> callFunction y (C_List (C_Cons (C_Builtin . BuiltinFunction "(continuation)" $ lift . extractSingleton >=> x) (C_List C_EmptyList))))
    ])

baseEnv :: IO Env
baseEnv = (fmap Map.fromList $ mapM (mapM $ newIORef . C_Builtin) (map (\(n, f) -> (n, BuiltinFunction n f)) builtins)) >>= newIORef
