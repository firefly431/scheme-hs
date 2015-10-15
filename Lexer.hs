module Lexer
( lex
, Token(..)
) where

import Prelude hiding (lex)

import Data.Char
import Types
import Data.Complex
import Data.Ratio
import Data.Maybe
import Data.List
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
           | T_CommentOne
           deriving (Show, Eq)

-- TODO: implement comments

special_initial = "!$%&*/:<=>?^_~"
special_subsequent = "+-.@"
special_delimiters = "()\";"
number_hashes = "iebodx"

delimits :: String -> Bool
delimits "" = True
delimits (c:s') = isSpace c || c `elem` special_delimiters

parse_rest :: String -> (Token, String)
parse_rest s@(c:s')
    | isLetter c || c `elem` special_initial = let (i, s'') = break (not . (\c -> isLetter c || isDigit c || c `elem` special_initial || c `elem` special_subsequent)) s' in (T_Identifier (c:i), s'')
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
    (T_String sr, s'') <- parse_string rest
    case c of
        Just ch -> return (T_String (ch : sr), s'')
        Nothing -> return (T_String sr, s'')
    where
        (c, rest) = case s' of
            'a':s'' -> (Just '\a', s'')
            'b':s'' -> (Just '\b', s'')
            't':s'' -> (Just '\t', s'')
            'n':s'' -> (Just '\n', s'')
            'v':s'' -> (Just '\v', s'')
            'f':s'' -> (Just '\f', s'')
            'r':s'' -> (Just '\r', s'')
            '"':s'' -> (Just '"', s'')
            '\\':s'' -> (Just '\\', s'')
            'x':s'' -> let (code, s''') = break (==';') s'' in (fmap chr . readMaybe . ("0x"++) $ code, tail s''')
            s'' -> (Nothing, snd . break (not . isSpace) $ s'')
parse_string (c:s') = do
    (T_String sr, s'') <- parse_string s'
    return (T_String (c : sr), s'')

data Radix = Decimal | Binary | Octal | Hexadecimal
    deriving (Enum, Show, Eq)

maybeBinaryDigit :: Char -> Maybe Integer
maybeBinaryDigit '0' = Just 0
maybeBinaryDigit '1' = Just 1
maybeBinaryDigit _ = Nothing

decodeInteger :: Radix -> String -> Maybe Integer
decodeInteger Decimal = readMaybe
decodeInteger Octal = readMaybe . ("0o"++)
decodeInteger Hexadecimal = readMaybe . ("0x"++)
decodeInteger Binary = foldl' (\a b -> do
    acc <- a
    dig <- maybeBinaryDigit b
    return ((acc * 2) + dig)) (Just 0) -- acc * 2 + dig

digits :: Radix -> String
digits Decimal = "0123456789"
digits Binary = "01"
digits Octal = "01234567"
digits Hexadecimal = "0123456789AaBbCcDdEeFf"

parse_number_prefix :: String -> (Radix, String)
parse_number_prefix ('#':'i':s') = parse_number_prefix s'
parse_number_prefix ('#':'e':s') = parse_number_prefix s'
parse_number_prefix ('#':'d':s') = let (_, s'') = parse_number_prefix s' in (Decimal, s'')
parse_number_prefix ('#':'b':s') = let (_, s'') = parse_number_prefix s' in (Binary, s'')
parse_number_prefix ('#':'o':s') = let (_, s'') = parse_number_prefix s' in (Octal, s'')
parse_number_prefix ('#':'x':s') = let (_, s'') = parse_number_prefix s' in (Hexadecimal, s'')
parse_number_prefix s = (Decimal, s)

parse_real :: Radix -> String -> Maybe (Double, String)
parse_real r ('+':s') = parse_ureal r s'
parse_real r ('-':s') = fmap (\(a, b) -> (negate a, b)) $ parse_ureal r s'
parse_real r s = parse_ureal r s

parse_ureal :: Radix -> String -> Maybe (Double, String)
parse_ureal r s = if p1 /= "" && s' /= "" && head s' == '/' then do
        p1i <- decodeInteger r p1
        p2i <- decodeInteger r p2
        return (fromRational (p1i % p2i), sp)
    else if (s' == "" || (not hasdp && (ex == "0" || r /= Decimal))) then do
        guard $ p1 /= ""
        result <- decodeInteger r p1
        return (fromIntegral result, s')
    else do
        guard $ hasdp || p1 /= ""
        guard $ r == Decimal
        let str = ((if p1 == "" then "0" else p1) ++ "." ++ dp ++ "e" ++ ex)
        result <- (readMaybe str :: Maybe Double)
        return (result, s''')
    where
        (p1, s') = parse_uint r s
        (p2, sp) = parse_uint r (tail s')
        (dp, s'', hasdp) = case s' of
            ('.':d) -> let (trail, rest) = parse_uint r d in if trail == "" then ("0", rest, False) else (trail, rest, True)
            something -> ("0", something, False)
        (ex, s''') = case s'' of
            ('e':d) -> parse_int r d
            ('E':d) -> parse_int r d
            something -> ("0", something)

parse_int :: Radix -> String -> (String, String)
parse_int r ('+':s') = let (n, s'') = parse_uint r s' in ('+':n, s'')
parse_int r ('-':s') = let (n, s'') = parse_uint r s' in ('-':n, s'')
parse_int r s = parse_uint r s

parse_uint :: Radix -> String -> (String, String)
parse_uint r = break (not . (`elem` (digits r)))

parse_number :: String -> Maybe (Token, String)
parse_number s = do
    let (radix, s') = parse_number_prefix s
    case parse_real radix s' of
        Just (n1, s'') ->
            case s'' of
                '@':s''' -> do
                    (n2, s'''') <- parse_real radix s'''
                    return (T_Number (n1 :+ n2), s'''')
                '+':s''' -> case parse_real radix s''' of
                    Just (n2, 'i':s'''') -> return (T_Number (n1 :+ n2), s'''')
                    _ -> if head s''' == 'i' then return (T_Number (n1 :+ 1), tail s''') else return (T_Number (n1 :+ 0), s'')
                '-':s''' -> case parse_real radix s''' of
                    Just (n2, 'i':s'''') -> return (T_Number (n1 :+ (negate n2)), s'''')
                    _ -> if head s''' == 'i' then return (T_Number (n1 :+ (-1)), tail s''') else return (T_Number (n1 :+ 0), s'')
                'i':s''' -> return (T_Number (0 :+ n1), s''')
                _ -> return (T_Number (n1 :+ 0), s'')
        Nothing -> do
            guard ((head s') `elem` "+-" && (head . tail $ s') == 'i')
            return (T_Number (0 :+ (if head s' == '+' then 1 else -1)), tail . tail $ s')

skipComment :: String -> String
skipComment ('-':'-':s') = tail . snd . break (=='\n') $ s'
skipComment ('#':'|':s') = skipBlock 1 s'
    where
        skipBlock 0 s = s
        skipBlock n ('#':'|':s'') = skipBlock (n + 1) s''
        skipBlock n ('|':'#':s'') = skipBlock (n - 1) s''
        skipBlock n (c:s'') = skipBlock n s''
skipComment s = s

lex :: String -> [Token]
lex s1 = if s == "" then [] else tok : lex rest
    where
        v = iterate (skipComment . snd . break (not . isSpace)) s1
        s = fst . head . dropWhile (uncurry (/=)) $ zip v (tail v)
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
                ';':s'' -> (T_CommentOne, s'')
                x:s'' -> if x `elem` number_hashes then fromJust . parse_number $ s else error "unknown hash"
                "" -> error "unexpected end of input"
            _ -> parse_rest (c:s')
