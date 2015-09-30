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
           | T_Number I_Number
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

parse_rest :: String -> Maybe (Token, String)
parse_rest s@(c:s')
    | isLetter c || c `elem` special_initial = let (i, s'') = break (not . (\c -> isLetter c || isDigit c || c `elem` special_initial || c `elem` special_subsequent)) s' in Just (T_Identifier i, s'')
    | delimits s' && c `elem` ".+-" = case c of
        '.' -> Just (T_Dot, s')
        '+' -> Just (T_Identifier "+", s')
        '-' -> Just (T_Identifier "-", s')
    | c == '.' && (head s' == '.') && (tail s' /= "") && ((head . tail $ s') == '.') = Just (T_Identifier "...", tail . tail $ s')
    | otherwise = parse_number s -- assume number

parse_string :: String -> Maybe (Token, String)
parse_string "" = fail "unexpected end of input"
parse_string ('"':s') = Just (T_String "", s')
parse_string ('\\':s') = let (T_String sr, s'') = parse_string . tail $ s' in Just (T_String (head s' : sr), s'')
parse_string (c:s') = let (T_String sr, s'') = parse_string s' in Just (T_String (c : sr), s'')

data Radix = Decimal | Binary | Octal | Hexadecimal
    deriving (Enum, Show)

parse_number_prefix :: String -> (Radix, String)
parse_number_prefix '#':'i':s' = parse_number_prefix s'
parse_number_prefix '#':'e':s' = parse_number_prefix s'
parse_number_prefix '#':'d':s' = let (_, s'') = parse_number_prefix s' in (Decimal, s'')
parse_number_prefix '#':'b':s' = let (_, s'') = parse_number_prefix s' in (Binary, s'')
parse_number_prefix '#':'o':s' = let (_, s'') = parse_number_prefix s' in (Octal, s'')
parse_number_prefix '#':'x':s' = let (_, s'') = parse_number_prefix s' in (Hexadecimal, s'')
parse_number_prefix s = (Decimal, s)

parse_real :: String -> Maybe (Token, String)
parse_real '+':s' = parse_ureal s'
parse_real '-':s' = fmap (\(T_Number num) -> (T_Number negate num)) parse_ureal s'

parse_number :: String -> Maybe (Token, String)
parse_number '+':'i':s' = Just (T_Number 0 :+ 1)
parse_number '-':'i':s' = Just (T_Number 0 :+ -1)
parse_number s =
    where
        (radix, s') = parse_number_prefix s

lex :: String -> [Token]
lex s1 = if s == "" then [] else tok : lex rest
    where
        s = snd . break (not . isSpace) $ s1
        (c:s') = s
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
