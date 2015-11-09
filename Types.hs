module Types
( S_Object(..)
, S_List(..)
, I_Number
, BuiltinFunction(..)
, S_Error(..)
, Env
, L_Closure(..)
, L_Program(..)
, ContT(..)
, SCont
, callCC
, display
, undefinedObject
, sappend
, equal
) where

import Data.Complex
import Data.Char(ord)
import Numeric(showHex)

import Control.Monad.Except

import Data.IORef
import qualified Data.Map.Strict as Map

type I_Number = Complex Double

data S_Error =
    User String
  | AssertionViolation String
  | NotFunction String
  | UndefinedVariable String
  | ArgumentError String
  | Syntax String
  | Other String

errorMessage (User s) = s
errorMessage (Other s) = s
errorMessage (AssertionViolation s) = "assertion violation: " ++ s
errorMessage (NotFunction s) = "not a function: " ++ s
errorMessage (UndefinedVariable s) = "undefined variable: " ++ s
errorMessage (ArgumentError s) = "argument error: " ++ s
errorMessage (Syntax s) = "syntax error: " ++ s

instance Show S_Error where
    show = ("error: "++) . errorMessage

instance Show BuiltinFunction where
    show = const "(builtin)"

type Env = IORef (Map.Map String (IORef S_Object))

newtype L_Closure = L_Closure { closure :: Env }

instance Show L_Closure where show = const "(closure)"

newtype L_Program = L_Program { runLambda :: (Env -> SCont) }

instance Show L_Program where show = const "(function)"

newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

type SCont = ContT () (ExceptT S_Error IO) S_Object

newtype BuiltinFunction = BuiltinFunction { runBuiltin :: S_Object -> SCont }

instance Functor m => Functor (ContT r m) where
    fmap f m = ContT $ \a -> runContT m (a . f)

instance Applicative m => Applicative (ContT r m) where
    pure x = ContT ($ x)
    f <*> a = ContT $ \b -> runContT f $ \c -> runContT a (b . c)

instance (Applicative m, Monad m) => Monad (ContT r m) where
    return x = ContT ($ x)
    a >>= b = ContT $ \c -> runContT a $ \d -> runContT (b d) c

instance MonadTrans (ContT r) where
    lift m = ContT (m >>=)

callCC :: ((a -> ContT r m a) -> ContT r m a) -> ContT r m a
callCC f = ContT $ \h -> runContT (f (\a -> ContT $ \_ -> h a)) h

data S_Object = C_Number I_Number
              | C_List S_List
              | C_Bool Bool
              | C_Char Char
              | C_Symbol String
              | C_String String
              | C_Builtin BuiltinFunction
              | C_Lambda L_Closure S_Object L_Program
              deriving (Show)

data S_List = C_EmptyList
            | C_Cons S_Object S_Object
            deriving (Show)

stringEscape :: String -> String
stringEscape ('\a':s') = "\\a" ++ stringEscape s'
stringEscape ('\b':s') = "\\b" ++ stringEscape s'
stringEscape ('\t':s') = "\\t" ++ stringEscape s'
stringEscape ('\n':s') = "\\n" ++ stringEscape s'
stringEscape ('\v':s') = "\\v" ++ stringEscape s'
stringEscape ('\f':s') = "\\f" ++ stringEscape s'
stringEscape ('\r':s') = "\\r" ++ stringEscape s'
stringEscape ('"':s') = "\\\"" ++ stringEscape s'
stringEscape ('\'':s') = "\\'" ++ stringEscape s'
stringEscape ('\\':s') = "\\\\" ++ stringEscape s'
stringEscape (c:s')
    | n >= 0x20 && n <= 0x7E = c:stringEscape s'
    | otherwise = "\\x" ++ (showHex n (stringEscape s')) ++ ";"
    where n = ord c
stringEscape "" = ""

displayList :: S_List -> String
displayList C_EmptyList = ""
displayList (C_Cons x (C_List C_EmptyList)) = display x
displayList (C_Cons x (C_List l)) = display x ++ " " ++ displayList l
displayList (C_Cons x y) = display x ++ " . " ++ display y

doubleIsIntegral :: Double -> Bool
doubleIsIntegral x = (x == (fromInteger . round $ x))

displayReal :: Double -> String
displayReal x
    | doubleIsIntegral x = show (round x)
    | otherwise = show x

displayNumber :: I_Number -> String
displayNumber (x :+ 0.0) = displayReal x
displayNumber (x :+ y) = displayReal x ++ "+" ++ displayReal y ++ "i"

display :: S_Object -> String
display (C_Number x) = displayNumber x
display (C_List x) = "(" ++ displayList x ++ ")"
display (C_Bool True) = "#t"
display (C_Bool False) = "#f"
display (C_Char '\n') = "#\\newline"
display (C_Char ' ') = "#\\space"
display (C_Char x) = '#' : '\\' : x : ""
display (C_Symbol x) = x
display (C_String x) = "\"" ++ (stringEscape x) ++ "\""
display (C_Builtin _) = "(builtin)"
display (C_Lambda _ p _) = "(lambda " ++ (display p) ++ "...)"

undefinedObject :: S_Object
undefinedObject = C_Bool False

sappend :: S_Object -> S_Object -> S_Object
sappend (C_List C_EmptyList) b = b
sappend (C_List (C_Cons a a')) b = C_List $ C_Cons a $ sappend a' b

equal :: S_Object -> S_Object -> Bool
equal a b = True -- TODO: fix
