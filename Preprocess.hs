module Preprocess
( preprocess
, preprocess_body
, base_context
, S_Program(..)
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
  | P_BuildEmptyList
  | P_BuildList S_Program S_Program
  | P_Undefined
    deriving (Show)

newtype S_Macro = C_Macro {rules :: [S_Rule]}

data S_Rule = C_Rule S_Pattern S_Pattern

data S_Pattern =
    P_Variable String
  | P_Const S_Object -- literal identifier included
  | P_EmptyList
  | P_Cons S_Pattern S_Pattern
  | P_Ellipsis S_Pattern

type S_Context = Map.Map String S_Macro

match_rule :: S_Object -> S_Rule -> Maybe (S_Pattern, Map.Map String S_Object)
match_rule obj (C_Rule pattern repl) = fmap ((,) repl) $ match_pattern obj pattern

match_pattern :: S_Object -> S_Pattern -> Maybe (Map.Map String S_Object)
match_pattern obj (P_Variable name) = Just $ Map.singleton name obj
match_pattern obj (P_Const lit)
    | equal lit obj = Just Map.empty
    | otherwise = Nothing
match_pattern (C_List C_EmptyList) P_EmptyList = Just Map.empty
match_pattern _ P_EmptyList = Nothing
match_pattern (C_List (C_Cons a b)) rule@(P_Cons c d) = case c of
    P_Ellipsis c' -> case match_pattern a c' of
        Just vars -> fmap (Map.unionWith (curry $ C_List . uncurry C_Cons) vars) $ match_pattern b rule
        Nothing -> match_pattern b d
    _ -> do
        a' <- match_pattern a c
        b' <- match_pattern b d
        return $ Map.union a' b'
match_pattern _ (P_Cons _ _) = Nothing
match_pattern _ (P_Ellipsis _) = Nothing

substitute_template :: S_Pattern -> Map.Map String S_Object -> S_Object
substitute_template (P_Variable name) vars = case Map.lookup name vars of
    Just val -> val
    Nothing -> C_Symbol name
substitute_template (P_Const val) _ = val
substitute_template (P_EmptyList) _ = C_List C_EmptyList
substitute_template (P_Cons a b) vars = case a of
    P_Ellipsis a' -> sappend (substitute_template a' vars) (substitute_template b vars)
    _ -> C_List $ C_Cons (substitute_template a vars) (substitute_template b vars)
substitute_template (P_Ellipsis _) _ = error "Invalid template"

base_context :: S_Context
base_context = Map.empty -- TODO: fill

preprocess :: S_Context -> S_Object -> S_Program
preprocess context x@(C_Number _) = P_Literal x
preprocess context x@(C_Bool _) = P_Literal x
preprocess context x@(C_Char _) = P_Literal x
preprocess context x@(C_String _) = P_Literal x
preprocess context (C_Symbol x) = P_Lookup x
preprocess context x@(C_List C_EmptyList) = P_Literal x -- ???
preprocess context (C_List (C_Cons a b)) = process_list context a b

process_args :: S_Context -> S_Object -> S_Program
process_args context (C_List C_EmptyList) = P_BuildEmptyList
process_args context (C_List (C_Cons a b)) = P_BuildList (preprocess context a) (preprocess context b)
process_args context x = preprocess context x

parse_transformer :: S_Object -> S_Macro
parse_transformer a = C_Macro [C_Rule P_EmptyList P_EmptyList] -- TODO: implement

preprocess_body :: S_Context -> S_Object -> S_Program
preprocess_body context (C_List (C_Cons a b)) = case a of
    (C_List (C_Cons (C_Symbol "define-syntax") (C_List (C_Cons (C_Symbol name) (C_List (C_Cons transformer (C_List C_EmptyList))))))) -> preprocess_body (Map.insert name (parse_transformer transformer) context) b
    a' -> case b of
        C_List C_EmptyList -> preprocess context a'
        b' -> P_Sequence (preprocess context a') $ preprocess_body context b'
preprocess_body _ _ = P_Undefined

expand_macro :: S_Macro -> S_Object -> S_Object
expand_macro macro obj = uncurry substitute_template $ head $ mapMaybe (match_rule obj) (rules macro)

process_list :: S_Context -> S_Object -> S_Object -> S_Program
process_list context (C_Symbol s) args = case s of
    "quote" -> case args of
        C_List (C_Cons a _) -> P_Literal a
        a -> P_Literal a
    "lambda" -> let (C_List (C_Cons (C_Symbol a) b)) = args in P_Procedure a (preprocess_body context b)
    "if" -> let (C_List (C_Cons a (C_List (C_Cons b c)))) = args in P_Conditional (preprocess context a) (preprocess context b) (preprocess_body context c)
    "set!" -> let (C_List (C_Cons (C_Symbol s) expr)) = args in P_Assignment s (preprocess context expr)
    name -> case Map.lookup name context of
        Nothing -> P_Call (P_Lookup name) (process_args context args)
        Just macro -> preprocess context $ expand_macro macro (C_List $ C_Cons (C_Symbol name) args)
process_list context func args = P_Call (preprocess context func) (process_args context args)
