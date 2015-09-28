module Lexer
( lex
, Token
) where

import Data.Char

data Token = T_Identifier String
           | T_Bool Bool
           | T_Number S_Number
           | T_Character Char
           | T_String String
           | T_Identifier String
           | T_LParen
           | T_RParen
           | T_VLParen
           | T_Quote
           | T_Backquote
           | T_Comma
           | T_CommaAt
           | T_Dot

special-initial = "!$%&*/:<=>?^_~"
special-subsequent = "+-.@"

lex :: String -> [Token]
lex "" = []
lex c:s' = tok : lex rest
    where (tok, rest) = case c of
        '(' -> (T_LParen, s')
        ')' -> (T_RParen, s')
        '\'' -> (T_Quote, s')
        '`' -> (T_Backquote, s')
        ',' -> if head s' == '@' then (T_CommaAt, tail s') else (T_Comma, s')
        '.' -> (T_Dot, s')
        '"' -> parse-string s'
        '#' -> case s' of
            '(':s'' -> (T_VLParen, s'') -- )
            't':s'' -> (T_Bool True, s'')
            'f':s'' -> (T_Bool False, s'')
            '\\':s'' -> case s'' of
                's':'p':'a':'c':'e':s''' ->  (T_Character ' ', s''')
                'n':'e':'w':'l':'i':'n':'e':s''' -> (T_Character '\n', s''')
                c:s''' -> (T_Character c, s''')
        _ -> case () of _
            | isLetter c || c `elem` special-initial = let (i, s'') = break (\c -> isLetter c || isDigit c || c `elem` special-initial || c `elem` special-subsequent) in (T_Identifier i, s'')
            | -- check for special: +, -, ...
