module Types
( S_Object(..)
, S_List(..)
, I_Number
, display
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

display :: S_Object -> String
display (C_Number x) = show x
display (C_List x) = "(" ++ displayList x ++ ")"
display (C_Bool True) = "#t"
display (C_Bool False) = "#f"
display (C_Char '\n') = "#\\newline"
display (C_Char ' ') = "#\\space"
display (C_Char x) = '#' : '\\' : x : ""
display (C_Symbol x) = x
display (C_String x) = "\"" ++ (stringEscape x) ++ "\""
