import System.Environment
import System.Exit
import System.IO

import Data.Map.Strict as Map

import Types
import Preprocess
import Parser

-- program to take a macro definition and output a definition

printUsageAndDie = getProgName >>= \x -> hPutStrLn stderr "USAGE:" >> hPutStr stderr x >> hPutStrLn stderr " [FILE]" >> exitFailure

getInputFile = getArgs >>= \x -> case x of
    [x] -> openFile x ReadMode
    [] -> return stdin
    _ -> printUsageAndDie

getDefinitions :: S_Object -> Map.Map String S_Macro
getDefinitions (C_List (C_Cons (C_List (C_Cons (C_Symbol "define-syntax") (C_List (C_Cons (C_Symbol name) (C_List (C_Cons transformer (C_List C_EmptyList))))))) xs)) = Map.insert name (parse_transformer transformer) (getDefinitions xs)
getDefinitions (C_List C_EmptyList) = Map.empty
getDefinitions _ = error "syntax error"

main = do
    code <- fmap parse $ getInputFile >>= hGetContents
    putStrLn . show $ getDefinitions code
