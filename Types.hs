module Types
( S_Object(..)
, S_Number(..)
, S_List(..)
, I_ExactNum
, I_InexactNum
) where

import Data.Complex
import Data.Ratio

type I_ExactNum = Complex (Ratio Integer)
type I_InexactNum = Complex Double

data S_Number = C_ExactNum I_ExactNum
              | C_InexactNum I_InexactNum
              deriving (Show)

data S_Object = C_Number S_Number
              | C_List S_Object S_Object
              | C_Bool Bool
              | C_Char Char
              | C_Symbol String
              | C_String String
              deriving (Show)

data S_List = C_EmptyList
            | C_Cons S_Object S_Object
