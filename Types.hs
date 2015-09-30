module Types
( S_Object(..)
, S_List(..)
, I_Number
) where

import Data.Complex

type I_Number = Complex Double

data S_Object = C_Number I_Number
              | C_List S_Object S_Object
              | C_Bool Bool
              | C_Char Char
              | C_Symbol String
              | C_String String
              deriving (Show)

data S_List = C_EmptyList
            | C_Cons S_Object S_Object
