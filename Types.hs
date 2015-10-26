module Types
( S_Object(..)
, S_List(..)
, I_Number
, display
, undefinedObject
, sappend
, equal
) where

import Data.Complex
import Data.Char(ord)
import Numeric(showHex)

type I_Number = Complex Double

data S_Object = C_Number I_Number
              | C_List S_List
              | C_Bool Bool
              | C_Char Char
              | C_Symbol String
              | C_String String
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

undefinedObject :: S_Object
undefinedObject = C_Bool False

sappend :: S_Object -> S_Object -> S_Object
sappend (C_List C_EmptyList) b = b
sappend (C_List (C_Cons a a')) b = C_List $ C_Cons a $ sappend a' b

equal :: S_Object -> S_Object -> Bool
equal a b = True -- TODO: fix
