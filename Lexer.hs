module Lexer
( lex
, Token
) where

import Prelude hiding (lex)

import Data.Char
import Types
import Data.Complex

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
           deriving (Show)

special_initial = "!$%&*/:<=>?^_~"
special_subsequent = "+-.@"
special_delimiters = "()\";"
number_hashes = "iebodx"

delimits :: String -> Bool
delimits "" = True
delimits (c:s') = isSpace c || c `elem` special_delimiters

lex :: String -> [Token]
lex "" = []
lex s@(c:s') = tok : lex rest
    where
        parse_rest :: String -> (Token, String)
        parse_rest (c:s')
            | isSpace c = parse_rest . snd . break isSpace $ s'
            | isLetter c || c `elem` special_initial = let (i, s'') = break (\c -> isLetter c || isDigit c || c `elem` special_initial || c `elem` special_subsequent) s' in (T_Identifier i, s'')
            | delimits s' = case c of
                '.' -> (T_Dot, s')
                '+' -> (T_Identifier "+", s')
                '-' -> (T_Identifier "-", s')
                _ -> error ("invalid character " ++ [c])
            | c == '.' && (head s' == '.') && (tail s' /= "") && ((head . tail $ s') == '.') = (T_Identifier "...", tail . tail $ s')
            | otherwise = parse_number s -- assume number
        parse_string "" = error "unexpected end of input"
        parse_string ('"':s') = (T_String "", s')
        parse_string ('\\':s') = let (T_String sr, s'') = parse_string . tail $ s' in (T_String (head s' : sr), s'')
        parse_string (c:s') = let (T_String sr, s'') = parse_string s' in (T_String (c : sr), s'')
        parse_number t = (T_Number (C_ExactNum (5 :+ 5)), tail t)
        (tok, rest) = case c of
            '(' -> (T_LParen, s')
            ')' -> (T_RParen, s')
            '\'' -> (T_Quote, s')
            '`' -> (T_Backquote, s')
            ',' -> if s' == "" then (T_Comma, s') else (if head s' == '@' then (T_CommaAt, tail s') else (T_Comma, s'))
            '"' -> parse_string s'
            '#' -> case s' of
                '(':s'' -> (T_VLParen, s'') -- )
                't':s'' -> (T_Bool True, s'')
                'f':s'' -> (T_Bool False, s'')
                '\\':s'' -> case s'' of
                    's':'p':'a':'c':'e':s''' ->  (T_Character ' ', s''')
                    'n':'e':'w':'l':'i':'n':'e':s''' -> (T_Character '\n', s''')
                    c:s''' -> (T_Character c, s''')
                x:s'' -> if x `elem` number_hashes then parse_number s else error "unknown hash"
                "" -> error "unexpected end of input"
            _ -> parse_rest (c:s')
