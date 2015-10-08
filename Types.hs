module Types
( S_Object(..)
, S_List(..)
, I_Number
, display
) where

import Data.Complex

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

stringEscape ('\a':s') = '\\':'a':stringEscape s'

display :: S_Object -> String
display C_Number x = show x
display C_List C_EmptyList = "()"
display C_List x = "(" ++ displayList x ++ ")"
display C_Bool True = "#t"
display C_Bool False = "#f"
display C_Char '\n' = "#\\newline"
display C_Char ' ' = "#\\space"
display C_Char x = '#' : '\\' : x : ""
display C_Symbol x = x
display C_String x = "\"" ++ (concat . map stringEscape $ x) ++ "\""
