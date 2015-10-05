module Lexer
( lex
, Token
) where

import Prelude hiding (lex)

import Data.Char
import Types
import Data.Complex
import Data.Ratio
import Data.Maybe
import Text.Read(readMaybe)
import Control.Monad

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

parse_rest :: String -> (Token, String)
parse_rest s@(c:s')
    | isLetter c || c `elem` special_initial = let (i, s'') = break (not . (\c -> isLetter c || isDigit c || c `elem` special_initial || c `elem` special_subsequent)) s' in (T_Identifier i, s'')
    | delimits s' && c `elem` ".+-" = case c of
        '.' -> (T_Dot, s')
        '+' -> (T_Identifier "+", s')
        '-' -> (T_Identifier "-", s')
    | c == '.' && (head s' == '.') && (tail s' /= "") && ((head . tail $ s') == '.') = (T_Identifier "...", tail . tail $ s')
    | otherwise = fromJust . parse_number $ s -- assume number

parse_string :: String -> Maybe (Token, String)
parse_string "" = fail "unexpected end of input"
parse_string ('"':s') = Just (T_String "", s')
parse_string ('\\':s') = do
    (T_String sr, s'') <- parse_string . tail $ s'
    Just (T_String (head s' : sr), s'')
parse_string (c:s') = do
    (T_String sr, s'') <- parse_string s'
    Just (T_String (c : sr), s'')

data Radix = Decimal | Binary | Octal | Hexadecimal
    deriving (Enum, Show)

parse_number_prefix :: String -> (Radix, String)
parse_number_prefix ('#':'i':s') = parse_number_prefix s'
parse_number_prefix ('#':'e':s') = parse_number_prefix s'
parse_number_prefix ('#':'d':s') = let (_, s'') = parse_number_prefix s' in (Decimal, s'')
parse_number_prefix ('#':'b':s') = let (_, s'') = parse_number_prefix s' in (Binary, s'')
parse_number_prefix ('#':'o':s') = let (_, s'') = parse_number_prefix s' in (Octal, s'')
parse_number_prefix ('#':'x':s') = let (_, s'') = parse_number_prefix s' in (Hexadecimal, s'')
parse_number_prefix s = (Decimal, s)

parse_real :: String -> Maybe (Double, String)
parse_real ('+':s') = parse_ureal s'
parse_real ('-':s') = fmap (\(a, b) -> (negate a, b)) $ parse_ureal s'
parse_real s = parse_ureal s

parse_ureal :: String -> Maybe (Double, String)
parse_ureal s = if p1 /= "" && s' /= "" && head s' == '/' then do
        p1i <- (readMaybe p1 :: Maybe Integer)
        p2i <- (readMaybe p2 :: Maybe Integer)
        return (fromRational (p1i % p2i), sp)
    else do
        guard $ p1 /= "" || hasdp
        let str = ((if p1 == "" then "0" else p1) ++ "." ++ dp ++ "e" ++ ex)
        result <- (readMaybe str :: Maybe Double)
        return (result, s''')
    where
        (p1, s') = parse_uint s
        (p2, sp) = parse_uint (tail s')
        (dp, s'', hasdp) = case s' of
            ('.':d) -> let (trail, rest) = parse_uint d in if trail == "" then ("0", rest, False) else (trail, rest, True)
            something -> ("0", something, False)
        (ex, s''') = case s'' of
            ('e':d) -> parse_int d
            ('E':d) -> parse_int d
            something -> ("0", something)

parse_int :: String -> (String, String)
parse_int ('+':s') = let (n, s'') = parse_uint s' in ('+':n, s'')
parse_int ('-':s') = let (n, s'') = parse_uint s' in ('-':n, s'')
parse_int s = parse_uint s

parse_uint :: String -> (String, String)
parse_uint = break (not . isDigit) -- TODO: handle hex

parse_number :: String -> Maybe (Token, String)
parse_number s = do
    let (radix, s') = parse_number_prefix s
    case parse_real s' of
        Just (n1, s'') ->
            case s'' of
                '@':s''' -> do
                    (n2, s'''') <- parse_real s'''
                    return (T_Number (n1 :+ n2), s'''')
                '+':s''' -> case parse_real s''' of
                    Just (n2, 'i':s'''') -> return (T_Number (n1 :+ n2), s'''')
                    _ -> if head s''' == 'i' then return (T_Number (n1 :+ 1), tail s''') else return (T_Number (n1 :+ 0), s'')
                '-':s''' -> case parse_real s''' of
                    Just (n2, 'i':s'''') -> return (T_Number (n1 :+ (negate n2)), s'''')
                    _ -> if head s''' == 'i' then return (T_Number (n1 :+ (-1)), tail s''') else return (T_Number (n1 :+ 0), s'')
                'i':s''' -> return (T_Number (0 :+ n1), s''')
                _ -> return (T_Number (n1 :+ 0), s'')
        Nothing -> do
            guard ((head s') `elem` "+-" && (head . tail $ s') == 'i')
            return (T_Number (0 :+ (if head s' == '+' then 1 else -1)), tail . tail $ s')

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
            '"' -> fromJust . parse_string $ s'
            '#' -> case s' of
                '(':s'' -> (T_VLParen, s'') -- )
                't':s'' -> (T_Bool True, s'')
                'f':s'' -> (T_Bool False, s'')
                '\\':s'' -> case s'' of
                    's':'p':'a':'c':'e':s''' ->  (T_Character ' ', s''')
                    'n':'e':'w':'l':'i':'n':'e':s''' -> (T_Character '\n', s''')
                    c:s''' -> (T_Character c, s''')
                x:s'' -> if x `elem` number_hashes then fromJust . parse_number $ s else error "unknown hash"
                "" -> error "unexpected end of input"
            _ -> parse_rest (c:s')
