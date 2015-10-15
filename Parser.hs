module Parser
( parse
) where

import Prelude hiding (lex)
import Data.Maybe

import Lexer
import Types

parse :: String -> S_Object
parse = parse' . lex

parse' :: [Token] -> S_Object
parse' [] = C_List C_EmptyList
parse' toks = case parse_one toks of
    Just (obj, rest) -> C_List $ C_Cons obj (parse' rest)
    Nothing -> C_List C_EmptyList

parse_one :: [Token] -> Maybe (S_Object, [Token])
parse_one [] = Nothing
parse_one (T_CommentOne : rest) = parse_one rest >>= parse_one . snd
parse_one (T_LParen : rest) = parse_list_body rest
    where
        parse_list_body :: [Token] -> Maybe (S_Object, [Token])
        parse_list_body (T_RParen : rest) = Just (C_List C_EmptyList, rest)
        parse_list_body (T_Dot : rest) = do
            (obj, rest') <- parse_one rest
            if head rest' /= T_RParen then error "expected RParen" else return (obj, tail rest')
        parse_list_body toks = do
            (obj, rest) <- parse_one toks
            (list, rest') <- parse_list_body rest
            return (C_List $ C_Cons obj list, rest')
-- substitute quote syntax for (quote something)
parse_one (T_Quote : rest) = do
    (obj, rest) <- parse_one rest
    return (C_List $ C_Cons (C_Symbol "quote") $ C_List $ C_Cons obj $ C_List C_EmptyList, rest)
