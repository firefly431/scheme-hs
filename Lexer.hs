module Lexer
( lex
, Token
) where

data Token = T_Identifier String
           | T_Bool Bool
           | T_Number S_Number
           | T_Character Char
           | T_String String
           | T_LParen
           | T_RParen
           | T_VLParen
           | T_Quote
           | T_Backquote
           | T_Comma
           | T_CommaAt
           | T_Dot

lex :: String -> [Token]
