module Preprocess
(preprocess
) where

import Types

data S_Program =
    P_Literal S_Object
  | P_Lookup String
  | P_Call S_Program S_Program
  | P_Procedure S_Object S_Program
  | P_Conditional S_Object S_Program S_Program
  | P_Assignment String S_Program
  | P_Sequence S_Program S_Program
  | P_Undefined

base_env :: S_Context

preprocess :: S_Context -> S_Object -> S_Program
preprocess context x@(C_Number _) = P_Literal x
preprocess context x@(C_Bool _) = P_Literal x
preprocess context x@(C_Char _) = P_Literal x
preprocess context x@(C_String _) = P_Literal x
preprocess context C_Symbol x = P_Lookup x
preprocess context x@(C_List C_EmptyList) = P_Literal x -- ???
preprocess context C_List (C_Cons a b) = process_list env a b

process_list :: S_Context -> S_Object -> S_Object -> S_Program
process_list context (C_Symbol s) args = case s of
    "quote" -> P_Literal args
    "lambda" -> let (C_List (C_Cons a b)) = args in P_Procedure a (process_body context b)
    "if" -> let (C_List (C_Cons a (C_Cons b c))) = args in P_Conditional a (preprocess context b) (preprocess context c) -- define result of (if #f ...) -> ()
