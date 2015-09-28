module Types
( S_Object(..)
, S_Number
, S_List
, S_Bool
, S_Char
, S_Symbol
, S_String
, I_ExactNum
, I_InexactNum
) where

import Data.Complex

type I_ExactNum = Complex (Ratio Integer)
type I_InexactNum = Complex Double

data S_Object = C_ExactNum I_ExactNum
              | C_InexactNum I_InexactNum
              | C_List S_Object S_Object
              | C_Bool Bool
              | C_Char Char
              | C_Symbol String
              | C_String String
              deriving (Show)
