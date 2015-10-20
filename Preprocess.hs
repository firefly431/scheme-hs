module Preprocess
(preprocess
) where

import Types
import qualified Data.Map.Strict as Map

data S_Program =
    P_Literal S_Object
  | P_Lookup String
  | P_Call S_Program S_Program
  | P_Procedure String S_Program
  | P_Conditional S_Program S_Program S_Program
  | P_Assignment String S_Program
  | P_Sequence S_Program S_Program
  | P_Undefined

data S_Macro = C_Macro [String] [S_Rule]

data S_Rule = C_Rule S_Pattern S_Pattern

data S_Pattern =
    P_Variable String
  | P_Literal String
  | P_Const S_Object
  | P_EmptyList
  | P_Cons S_Pattern S_Pattern
  | P_Ellipsis S_Pattern

type S_Context = Map.Map String S_Macro

match_pattern :: [String] -> S_Pattern -> S_Object -> Maybe (Map.Map String S_Object)

substitute_template :: S_Pattern -> Map.Map String S_Object -> S_Object -> S_Object

base_env :: S_Context

preprocess :: S_Context -> S_Object -> S_Program
preprocess context x@(C_Number _) = P_Literal x
preprocess context x@(C_Bool _) = P_Literal x
preprocess context x@(C_Char _) = P_Literal x
preprocess context x@(C_String _) = P_Literal x
preprocess context C_Symbol x = P_Lookup x
preprocess context x@(C_List C_EmptyList) = P_Literal x -- ???
preprocess context C_List (C_Cons a b) = process_list env a b

preprocess_body :: Context -> S_Object -> S_Program

process_list :: S_Context -> S_Object -> S_Object -> S_Program
process_list context (C_Symbol s) args = case s of
    "quote" -> P_Literal args
    "lambda" -> let (C_List (C_Cons (C_Symbol a) b)) = args in P_Procedure a (process_body context b)
    "if" -> let (C_List (C_Cons a (C_List (C_Cons b c)))) = args in P_Conditional (preprocess context a) (preprocess context b) (preprocess_body context c)
    "set!" -> let (C_List (C_Cons (C_Symbol s) expr)) = args in P_Assignment s (preprocess context expr)
    name -> case Map.lookup name context of
        Nothing -> P_Call (P_Lookup name) args
        Just macro -> expand_macro context macro args
