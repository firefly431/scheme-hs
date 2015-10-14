module Parser
( parse
) where

import Prelude hiding (lex)

import Lexer
import Types

parse :: String -> S_Object
parse = parse' . lex

parse' :: [Token] -> S_Object
parse' [] = C_List C_EmptyList
parse' toks = let (obj, rest) = parse_one toks in C_List $ C_Cons obj (parse' rest)

parse_one :: [Token] -> (S_Object, [Token])
parse_one (T_LParen : rest) = parse_list_body rest
    where
        parse_list_body :: [Token] -> (S_Object, [Token])
        parse_list_body (T_RParen : rest) = (C_List C_EmptyList, rest)
        parse_list_body (T_Dot : rest) = let (obj, rest') = parse_one rest in if head rest' /= T_RParen then error "expected RParen" else (obj, tail rest')
        parse_list_body toks = let
            (obj, rest) = parse_one toks
            (list, rest') = parse_list_body rest in (C_List $ C_Cons obj list, rest')
-- substitute quote syntax for (quote something)
parse_one (T_Quote : rest) = let (obj, rest) = parse_one rest in (C_List $ C_Cons (C_Symbol "quote") $ C_List $ C_Cons obj $ C_List C_EmptyList, rest)
