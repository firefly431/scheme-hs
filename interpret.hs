import System.IO
import System.Environment
import System.Exit
import Control.Monad.Except

import Types
import Eval
import Builtins

printUsageAndDie = getProgName >>= \x -> hPutStrLn stderr "USAGE:" >> hPutStr stderr x >> hPutStrLn stderr " [FILE]" >> exitFailure

getInputFile = getArgs >>= \x -> case x of
    [x] -> openFile x ReadMode
    [] -> return stdin
    _ -> printUsageAndDie

main = do
    env <- baseEnv
    code <- getInputFile >>= hGetContents
    res <- runExceptT $ runContT (interpret env code) (const $ return ())
    case res of
        Left e -> print e
        Right _ -> return ()
