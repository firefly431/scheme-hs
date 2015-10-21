module Preprocess
(preprocess
) where

import Types
import Data.Maybe
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

newtype S_Macro = C_Macro {rules :: [S_Rule]}

data S_Rule = C_Rule S_Pattern S_Pattern

data S_Pattern =
    P_Variable String
  | P_Literal String
  | P_Const S_Object
  | P_EmptyList
  | P_Cons S_Pattern S_Pattern
  | P_Ellipsis S_Pattern

type S_Context = Map.Map String S_Macro

match_rule :: S_Object -> S_Rule -> Maybe (S_Pattern, Map.Map String S_Object)
match_rule obj (C_Rule pattern repl) = fmap ((,) repl) names
    where
        names = case pattern of
            P_Variable name -> -- singleton
            P_Literal lit -> -- if it matches, Just, else Nothing

substitute_template :: S_Pattern -> Map.Map String S_Object -> S_Object

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
preprocess_body context C_List (C_Cons a b) = case a of
    (C_List (C_Cons (C_Symbol "define-syntax") (C_List $ C_Cons (C_Symbol name) (C_List $ C_Cons transformer (C_List C_EmptyList))))) -> preprocess_body (augment_context context name transformer) b
    a' -> P_Sequence (preprocess context a') $ preprocess_body context b
preprocess_body _ _ = undefinedObject

expand_macro :: S_Macro -> S_Object -> S_Object
expand_macro macro obj = uncurry substitute_template $ head $ mapMaybe (match_pattern obj) (rules macro)

process_list :: S_Context -> S_Object -> S_Object -> S_Program
process_list context (C_Symbol s) args = case s of
    "quote" -> P_Literal args
    "lambda" -> let (C_List (C_Cons (C_Symbol a) b)) = args in P_Procedure a (process_body context b)
    "if" -> let (C_List (C_Cons a (C_List (C_Cons b c)))) = args in P_Conditional (preprocess context a) (preprocess context b) (preprocess_body context c)
    "set!" -> let (C_List (C_Cons (C_Symbol s) expr)) = args in P_Assignment s (preprocess context expr)
    name -> case Map.lookup name context of
        Nothing -> P_Call (P_Lookup name) (process_args args)
        Just macro -> preprocess context $ expand_macro macro (process_args args)
process_list context func args = P_Call (preprocess context) (process_args args)
