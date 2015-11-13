module Preprocess
( preprocess
, preprocess_body
, base_context
, parse_transformer
, S_Program(..)
, S_Macro(..)
) where

import Types
import Data.Maybe
import qualified Data.Map.Strict as Map

import Debug.Trace

data S_Program =
    P_Literal S_Object
  | P_Lookup String
  | P_Call S_Program S_Program
  | P_Procedure S_Object S_Program
  | P_Conditional S_Program S_Program S_Program
  | P_Assignment String S_Program
  | P_Definition String S_Program
  | P_Sequence S_Program S_Program
  | P_BuildEmptyList
  | P_BuildList S_Program S_Program
  | P_Undefined
    deriving (Show)

newtype S_Macro = C_Macro {rules :: [S_Rule]}
    deriving (Show)

data S_Rule = C_Rule S_Pattern S_Pattern
    deriving (Show)

data S_Pattern =
    P_Variable String
  | P_Const S_Object -- literal identifier included
  | P_EmptyList
  | P_Cons S_Pattern S_Pattern
  | P_Ellipsis S_Pattern
    deriving (Show)

type S_Context = Map.Map String S_Macro

data P_Var =
    P_Object S_Object
  | P_ECons P_Var P_Var
  | P_Empty

match_rule :: S_Object -> S_Rule -> Maybe (S_Pattern, Map.Map String P_Var)
match_rule obj (C_Rule pattern repl) = fmap ((,) repl) $ match_pattern obj pattern

fill_symbols :: S_Pattern -> Map.Map String P_Var
fill_symbols (P_Variable name) = Map.singleton name $ P_Object (C_List C_EmptyList)
fill_symbols (P_Const lit) = Map.empty
fill_symbols (P_EmptyList) = Map.empty
fill_symbols (P_Cons a b) = Map.union (fill_symbols b) (fill_symbols a)
fill_symbols (P_Ellipsis a) = fill_symbols a

match_pattern :: S_Object -> S_Pattern -> Maybe (Map.Map String P_Var)
match_pattern obj (P_Variable name) = Just $ Map.singleton name $ P_Object obj
match_pattern obj (P_Const lit)
    | equal lit obj = Just Map.empty
    | otherwise = Nothing
match_pattern (C_List C_EmptyList) P_EmptyList = Just Map.empty
match_pattern _ P_EmptyList = Nothing
match_pattern (C_List C_EmptyList) (P_Cons (P_Ellipsis a) P_EmptyList) = Just $ fill_symbols a
match_pattern (C_List (C_Cons a b)) rule@(P_Cons c d) = case c of
    P_Ellipsis c' -> case match_pattern a c' of
        Just vars -> fmap (Map.unionWith (P_ECons) vars) $ match_pattern b rule
        Nothing -> match_pattern b d
    _ -> do
        a' <- match_pattern a c
        b' <- match_pattern b d
        return $ Map.union b' a' -- prefer right
match_pattern _ (P_Cons _ _) = Nothing
match_pattern _ (P_Ellipsis _) = Nothing

getFirst :: P_Var -> Maybe S_Object
getFirst (P_Object a) = Just a
getFirst (P_ECons a b) = case getFirst a of
    Just val -> Just val
    Nothing -> getFirst b
getFirst (P_Empty) = Nothing

unwrapOne :: Map.Map String P_Var -> Map.Map String P_Var
unwrapOne = Map.map unwrapOne'
    where unwrapOne' (P_Object a) = P_Object a
          unwrapOne' (P_ECons a b) = b
          unwrapOne' (P_Empty) = P_Empty

substitute_template :: S_Pattern -> Map.Map String P_Var -> Maybe S_Object
substitute_template (P_Variable name) vars = case Map.lookup name vars of
    Just val -> getFirst val
    Nothing -> Just $ C_Symbol name
substitute_template (P_Const val) _ = Just val
substitute_template (P_EmptyList) _ = Just $ C_List C_EmptyList
substitute_template rule@(P_Cons a b) vars = case a of
    P_Ellipsis a' -> case substitute_template a' vars of
        Just a'' -> Just $ C_List $ C_Cons a'' (fromJust $ substitute_template rule (unwrapOne vars))
        Nothing -> substitute_template b vars
    _ -> do
        a' <- substitute_template a vars
        b' <- substitute_template b vars
        return $ C_List $ C_Cons a' b'
substitute_template (P_Ellipsis _) _ = error "invalid template"

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
process_args context (C_List (C_Cons a b)) = P_BuildList (preprocess context a) (process_args context b)
process_args context x = preprocess context x

parse_transformer :: S_Object -> S_Macro
parse_transformer a = case a of
    (C_List (C_Cons (C_Symbol "syntax-rules") (C_List (C_Cons lits@(C_List _) rules@(C_List _))))) -> C_Macro $ parse_rules (parse_lits lits) rules
    _ -> error "invalid transformer"

parse_lits :: S_Object -> [String]
parse_lits (C_List (C_Cons (C_Symbol x) xs)) = x : (parse_lits xs)
parse_lits (C_List C_EmptyList) = []
parse_lits _ = error "invalid literals"

parse_rules :: [String] -> S_Object -> [S_Rule]
parse_rules lits (C_List (C_Cons x xs)) = (parse_rule lits x) : (parse_rules lits xs)
parse_rules lits (C_List (C_EmptyList)) = []

parse_rule :: [String] -> S_Object -> S_Rule
parse_rule lits (C_List (C_Cons pat (C_List (C_Cons temp (C_List C_EmptyList))))) = C_Rule (parse_pattern lits pat) (parse_pattern [] temp)
parse_rule _ _ = error "invalid rule"

parse_pattern :: [String] -> S_Object -> S_Pattern
parse_pattern lits x@(C_Number _) = P_Const x
parse_pattern lits x@(C_Bool _) = P_Const x
parse_pattern lits x@(C_Char _) = P_Const x
parse_pattern lits x@(C_String _) = P_Const x
parse_pattern lits (C_Symbol x) = if (x `elem` lits) then (P_Const (C_Symbol x)) else (P_Variable x)
parse_pattern lits (C_List C_EmptyList) = P_EmptyList
parse_pattern lits (C_List (C_Cons a b)) = case b of
    (C_List (C_Cons (C_Symbol "...") c)) -> P_Cons (P_Ellipsis (parse_pattern lits a)) (parse_pattern lits c)
    _ -> P_Cons (parse_pattern lits a) (parse_pattern lits b)

preprocess_body :: S_Context -> S_Object -> S_Program
preprocess_body context (C_List (C_Cons a b)) = case a of
    (C_List (C_Cons (C_Symbol "define-syntax") (C_List (C_Cons (C_Symbol name) (C_List (C_Cons transformer (C_List C_EmptyList))))))) -> preprocess_body (Map.insert name (parse_transformer transformer) context) b
    a' -> case b of
        C_List C_EmptyList -> preprocess context a'
        b' -> P_Sequence (preprocess context a') $ preprocess_body context b'
preprocess_body _ _ = P_Undefined

traceThis :: (Show a) => String -> (a -> String) -> a -> a
traceThis msg show x = trace (msg ++ show x) x

expand_macro :: S_Macro -> S_Object -> S_Object
expand_macro macro obj = traceThis ("expanded " ++ (display obj) ++ " to ") display $ head $ mapMaybe (\x -> match_rule obj x >>= uncurry substitute_template) (rules macro)

process_list :: S_Context -> S_Object -> S_Object -> S_Program
process_list context (C_Symbol s) args = case s of
    "quote" -> case args of
        C_List (C_Cons a _) -> P_Literal a
        a -> P_Literal a
    "lambda" -> let (C_List (C_Cons a b)) = args in P_Procedure a (preprocess_body context b)
    "if" -> let (C_List (C_Cons a (C_List (C_Cons b c)))) = args in P_Conditional (preprocess context a) (preprocess context b) (preprocess_body context c)
    "set!" -> let (C_List (C_Cons (C_Symbol s) expr)) = args in P_Assignment s (preprocess_body context expr)
    "define" -> let (C_List (C_Cons (C_Symbol s) expr)) = args in P_Definition s (preprocess_body context expr)
    "begin" -> preprocess_body context args
    name -> case Map.lookup name context of
        Nothing -> P_Call (P_Lookup name) (process_args context args)
        Just macro -> preprocess context $ expand_macro macro (C_List $ C_Cons (C_Symbol name) args)
process_list context func args = P_Call (preprocess context func) (process_args context args)

base_context :: S_Context
base_context = Map.fromList $
    [ ("and",C_Macro {rules = [C_Rule (P_Cons (P_Variable
"and") P_EmptyList) (P_Const (C_Bool True)),C_Rule (P_Cons (P_Variable "and")
(P_Cons (P_Variable "test") P_EmptyList)) (P_Variable "test"),C_Rule (P_Cons
(P_Variable "and") (P_Cons (P_Variable "test1") (P_Cons (P_Ellipsis
(P_Variable "test2")) P_EmptyList))) (P_Cons (P_Variable "if") (P_Cons
(P_Variable "test1") (P_Cons (P_Cons (P_Variable "and") (P_Cons (P_Ellipsis
(P_Variable "test2")) P_EmptyList)) (P_Cons (P_Const (C_Bool False))
P_EmptyList))))]})
    , ("case",C_Macro {rules = [C_Rule (P_Cons (P_Variable
"case") (P_Cons (P_Cons (P_Ellipsis (P_Variable "key")) P_EmptyList) (P_Cons
(P_Ellipsis (P_Variable "clauses")) P_EmptyList))) (P_Cons (P_Variable "let")
(P_Cons (P_Cons (P_Cons (P_Variable "atom-key") (P_Cons (P_Cons (P_Ellipsis
(P_Variable "key")) P_EmptyList) P_EmptyList)) P_EmptyList) (P_Cons (P_Cons
(P_Variable "case") (P_Cons (P_Variable "atom-key") (P_Cons (P_Ellipsis
(P_Variable "clauses")) P_EmptyList))) P_EmptyList))),C_Rule (P_Cons
(P_Variable "case") (P_Cons (P_Variable "key") (P_Cons (P_Cons (P_Const
(C_Symbol "else")) (P_Cons (P_Variable "result1") (P_Cons (P_Ellipsis
(P_Variable "result2")) P_EmptyList))) P_EmptyList))) (P_Cons (P_Variable
"begin") (P_Cons (P_Variable "result1") (P_Cons (P_Ellipsis (P_Variable
"result2")) P_EmptyList))),C_Rule (P_Cons (P_Variable "case") (P_Cons
(P_Variable "key") (P_Cons (P_Cons (P_Cons (P_Ellipsis (P_Variable "atoms"))
P_EmptyList) (P_Cons (P_Variable "result1") (P_Cons (P_Ellipsis (P_Variable
"result2")) P_EmptyList))) P_EmptyList))) (P_Cons (P_Variable "if") (P_Cons
(P_Cons (P_Variable "memv") (P_Cons (P_Variable "key") (P_Cons (P_Cons
(P_Variable "quote") (P_Cons (P_Cons (P_Ellipsis (P_Variable "atoms"))
P_EmptyList) P_EmptyList)) P_EmptyList))) (P_Cons (P_Cons (P_Variable "begin")
(P_Cons (P_Variable "result1") (P_Cons (P_Ellipsis (P_Variable "result2"))
P_EmptyList))) P_EmptyList))),C_Rule (P_Cons (P_Variable "case") (P_Cons
(P_Variable "key") (P_Cons (P_Cons (P_Cons (P_Ellipsis (P_Variable "atoms"))
P_EmptyList) (P_Cons (P_Variable "result1") (P_Cons (P_Ellipsis (P_Variable
"result2")) P_EmptyList))) (P_Cons (P_Variable "clause") (P_Cons (P_Ellipsis
(P_Variable "clauses")) P_EmptyList))))) (P_Cons (P_Variable "if") (P_Cons
(P_Cons (P_Variable "memv") (P_Cons (P_Variable "key") (P_Cons (P_Cons
(P_Variable "quote") (P_Cons (P_Cons (P_Ellipsis (P_Variable "atoms"))
P_EmptyList) P_EmptyList)) P_EmptyList))) (P_Cons (P_Cons (P_Variable "begin")
(P_Cons (P_Variable "result1") (P_Cons (P_Ellipsis (P_Variable "result2"))
P_EmptyList))) (P_Cons (P_Cons (P_Variable "case") (P_Cons (P_Variable "key")
(P_Cons (P_Variable "clause") (P_Cons (P_Ellipsis (P_Variable "clauses"))
P_EmptyList)))) P_EmptyList))))]})
    , ("cond",C_Macro {rules = [C_Rule (P_Cons
(P_Variable "cond") (P_Cons (P_Cons (P_Const (C_Symbol "else")) (P_Cons
(P_Variable "result1") (P_Cons (P_Ellipsis (P_Variable "result2"))
P_EmptyList))) P_EmptyList)) (P_Cons (P_Variable "begin") (P_Cons (P_Variable
"result1") (P_Cons (P_Ellipsis (P_Variable "result2")) P_EmptyList))),C_Rule
(P_Cons (P_Variable "cond") (P_Cons (P_Cons (P_Variable "test") (P_Cons
(P_Const (C_Symbol "=>")) (P_Cons (P_Variable "result") P_EmptyList)))
P_EmptyList)) (P_Cons (P_Variable "let") (P_Cons (P_Cons (P_Cons (P_Variable
"temp") (P_Cons (P_Variable "test") P_EmptyList)) P_EmptyList) (P_Cons (P_Cons
(P_Variable "if") (P_Cons (P_Variable "temp") (P_Cons (P_Cons (P_Variable
"result") (P_Cons (P_Variable "temp") P_EmptyList)) P_EmptyList)))
P_EmptyList))),C_Rule (P_Cons (P_Variable "cond") (P_Cons (P_Cons (P_Variable
"test") (P_Cons (P_Const (C_Symbol "=>")) (P_Cons (P_Variable "result")
P_EmptyList))) (P_Cons (P_Variable "clause1") (P_Cons (P_Ellipsis (P_Variable
"clause2")) P_EmptyList)))) (P_Cons (P_Variable "let") (P_Cons (P_Cons (P_Cons
(P_Variable "temp") (P_Cons (P_Variable "test") P_EmptyList)) P_EmptyList)
(P_Cons (P_Cons (P_Variable "if") (P_Cons (P_Variable "temp") (P_Cons (P_Cons
(P_Variable "result") (P_Cons (P_Variable "temp") P_EmptyList)) (P_Cons
(P_Cons (P_Variable "cond") (P_Cons (P_Variable "clause1") (P_Cons (P_Ellipsis
(P_Variable "clause2")) P_EmptyList))) P_EmptyList)))) P_EmptyList))),C_Rule
(P_Cons (P_Variable "cond") (P_Cons (P_Cons (P_Variable "test") P_EmptyList)
P_EmptyList)) (P_Variable "test"),C_Rule (P_Cons (P_Variable "cond") (P_Cons
(P_Cons (P_Variable "test") P_EmptyList) (P_Cons (P_Variable "clause1")
(P_Cons (P_Ellipsis (P_Variable "clause2")) P_EmptyList)))) (P_Cons
(P_Variable "let") (P_Cons (P_Cons (P_Cons (P_Variable "temp") (P_Cons
(P_Variable "test") P_EmptyList)) P_EmptyList) (P_Cons (P_Cons (P_Variable
"if") (P_Cons (P_Variable "temp") (P_Cons (P_Variable "temp") (P_Cons (P_Cons
(P_Variable "cond") (P_Cons (P_Variable "clause1") (P_Cons (P_Ellipsis
(P_Variable "clause2")) P_EmptyList))) P_EmptyList)))) P_EmptyList))),C_Rule
(P_Cons (P_Variable "cond") (P_Cons (P_Cons (P_Variable "test") (P_Cons
(P_Variable "result1") (P_Cons (P_Ellipsis (P_Variable "result2"))
P_EmptyList))) P_EmptyList)) (P_Cons (P_Variable "if") (P_Cons (P_Variable
"test") (P_Cons (P_Cons (P_Variable "begin") (P_Cons (P_Variable "result1")
(P_Cons (P_Ellipsis (P_Variable "result2")) P_EmptyList)))
P_EmptyList))),C_Rule (P_Cons (P_Variable "cond") (P_Cons (P_Cons (P_Variable
"test") (P_Cons (P_Variable "result1") (P_Cons (P_Ellipsis (P_Variable
"result2")) P_EmptyList))) (P_Cons (P_Variable "clause1") (P_Cons (P_Ellipsis
(P_Variable "clause2")) P_EmptyList)))) (P_Cons (P_Variable "if") (P_Cons
(P_Variable "test") (P_Cons (P_Cons (P_Variable "begin") (P_Cons (P_Variable
"result1") (P_Cons (P_Ellipsis (P_Variable "result2")) P_EmptyList))) (P_Cons
(P_Cons (P_Variable "cond") (P_Cons (P_Variable "clause1") (P_Cons (P_Ellipsis
(P_Variable "clause2")) P_EmptyList))) P_EmptyList))))]})
    , ("do",C_Macro {rules = [C_Rule (P_Cons (P_Variable "do") (P_Cons (P_Cons (P_Ellipsis (P_Cons (P_Variable "var") (P_Cons (P_Variable "init") (P_Cons
(P_Ellipsis (P_Variable "step")) P_EmptyList)))) P_EmptyList) (P_Cons (P_Cons (
P_Variable "test") (P_Cons (P_Ellipsis (P_Variable "expr")) P_EmptyList)) (P_Cons (P_Ellipsis (P_Variable "command")) P_EmptyList))))
(P_Cons (P_Variable "letrec") (P_Cons (P_Cons (P_Cons (P_Variable "loop") (P_Cons (P_Cons (P_Variable "lambda") (P_Cons (P_Cons (P_Ellipsis (P_Variable "var")) P_EmptyList) (P_Cons (
P_Cons (P_Variable "if") (P_Cons (P_Variable "test") (P_Cons (P_Cons (P_Variable "begin") (P_Cons (P_Cons (P_Variable "if") (P_Cons (P_Const (C_Bool False)) (
P_Cons (P_Const (C_Bool False)) P_EmptyList))) (P_Cons (P_Ellipsis (P_Variable
"expr")) P_EmptyList))) (P_Cons (P_Cons (P_Variable "begin") (P_Cons (P_Ellipsis
(P_Variable "command")) (P_Cons (P_Cons (P_Variable "loop") (P_Cons (P_Ellipsis
(P_Cons (P_Variable "do") (P_Cons (P_Const (C_String "step")) (P_Cons (P_Variable
"var") (P_Cons (P_Ellipsis (P_Variable "step")) P_EmptyList))))) P_EmptyList)) P_EmptyList))) P_EmptyList)))) P_EmptyList))) P_EmptyList)) P_EmptyList)
(P_Cons (P_Cons (P_Variable "loop") (P_Cons (P_Ellipsis (P_Variable "init")) P_EmptyList)) P_EmptyList))),C_Rule (P_Cons (P_Variable "do") (P_Cons (P_Const (C_String "step"))
(P_Cons (P_Variable "x") P_EmptyList))) (P_Variable "x"),C_Rule (P_Cons (P_Variable "do") (P_Cons (P_Const (C_String "step"))
(P_Cons (P_Variable "x") (P_Cons (P_Variable "y") P_EmptyList)))) (P_Variable "y")]})
    , ("let",C_Macro {rules = [C_Rule (P_Cons (P_Variable "let") (P_Cons (P_Cons (P_Ellipsis
(P_Cons (P_Variable "name") (P_Cons (P_Variable "val") P_EmptyList)))
P_EmptyList) (P_Cons (P_Variable "body1") (P_Cons (P_Ellipsis (P_Variable
"body2")) P_EmptyList)))) (P_Cons (P_Cons (P_Variable "lambda") (P_Cons
(P_Cons (P_Ellipsis (P_Variable "name")) P_EmptyList) (P_Cons (P_Variable
"body1") (P_Cons (P_Ellipsis (P_Variable "body2")) P_EmptyList)))) (P_Cons
(P_Ellipsis (P_Variable "val")) P_EmptyList)),C_Rule (P_Cons (P_Variable
"let") (P_Cons (P_Variable "tag") (P_Cons (P_Cons (P_Ellipsis (P_Cons
(P_Variable "name") (P_Cons (P_Variable "val") P_EmptyList))) P_EmptyList)
(P_Cons (P_Variable "body1") (P_Cons (P_Ellipsis (P_Variable "body2"))
P_EmptyList))))) (P_Cons (P_Cons (P_Variable "letrec") (P_Cons (P_Cons (P_Cons
(P_Variable "tag") (P_Cons (P_Cons (P_Variable "lambda") (P_Cons (P_Cons
(P_Ellipsis (P_Variable "name")) P_EmptyList) (P_Cons (P_Variable "body1")
(P_Cons (P_Ellipsis (P_Variable "body2")) P_EmptyList)))) P_EmptyList))
P_EmptyList) (P_Cons (P_Variable "tag") P_EmptyList))) (P_Cons (P_Ellipsis
(P_Variable "val")) P_EmptyList))]})
    , ("let*",C_Macro {rules = [C_Rule (P_Cons
(P_Variable "let*") (P_Cons P_EmptyList (P_Cons (P_Variable "body1") (P_Cons
(P_Ellipsis (P_Variable "body2")) P_EmptyList)))) (P_Cons (P_Variable "let")
(P_Cons P_EmptyList (P_Cons (P_Variable "body1") (P_Cons (P_Ellipsis
(P_Variable "body2")) P_EmptyList)))),C_Rule (P_Cons (P_Variable "let*")
(P_Cons (P_Cons (P_Cons (P_Variable "name1") (P_Cons (P_Variable "val1")
P_EmptyList)) (P_Cons (P_Ellipsis (P_Cons (P_Variable "name2") (P_Cons
(P_Variable "val2") P_EmptyList))) P_EmptyList)) (P_Cons (P_Variable "body1")
(P_Cons (P_Ellipsis (P_Variable "body2")) P_EmptyList)))) (P_Cons (P_Variable
"let") (P_Cons (P_Cons (P_Cons (P_Variable "name1") (P_Cons (P_Variable
"val1") P_EmptyList)) P_EmptyList) (P_Cons (P_Cons (P_Variable "let*") (P_Cons
(P_Cons (P_Ellipsis (P_Cons (P_Variable "name2") (P_Cons (P_Variable "val2")
P_EmptyList))) P_EmptyList) (P_Cons (P_Variable "body1") (P_Cons (P_Ellipsis
(P_Variable "body2")) P_EmptyList)))) P_EmptyList)))]})
    , ("letrec",C_Macro {rules = [C_Rule (P_Cons (P_Variable "_") (P_Cons (P_Cons (P_Ellipsis
 (P_Cons (P_Variable "var") (P_Cons (P_Variable "init") P_EmptyList))) P_EmptyList)
 (P_Variable "body"))) (P_Cons (P_Variable "let") (P_Cons P_EmptyList (P_Cons
 (P_Ellipsis (P_Cons (P_Variable "define") (P_Cons (P_Variable "var") (P_Cons
 (P_Variable "init") P_EmptyList)))) (P_Cons (P_Cons (P_Variable "let") (P_Cons
 P_EmptyList (P_Variable "body"))) P_EmptyList))))]})
    , ("or",C_Macro {rules =
[C_Rule (P_Cons (P_Variable "or") P_EmptyList) (P_Const (C_Bool False)),C_Rule
(P_Cons (P_Variable "or") (P_Cons (P_Variable "test") P_EmptyList))
(P_Variable "test"),C_Rule (P_Cons (P_Variable "or") (P_Cons (P_Variable
"test1") (P_Cons (P_Ellipsis (P_Variable "test2")) P_EmptyList))) (P_Cons
(P_Variable "let") (P_Cons (P_Cons (P_Cons (P_Variable "x") (P_Cons
(P_Variable "test1") P_EmptyList)) P_EmptyList) (P_Cons (P_Cons (P_Variable
"if") (P_Cons (P_Variable "x") (P_Cons (P_Variable "x") (P_Cons (P_Cons
(P_Variable "or") (P_Cons (P_Ellipsis (P_Variable "test2")) P_EmptyList))
P_EmptyList)))) P_EmptyList)))]})
    ]
