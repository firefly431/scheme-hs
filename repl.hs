import System.IO
import Control.Monad.Except

import Types
import Eval

main' env = do
    hPutStr stderr "scheme> "
    hFlush stderr
    line <- getLine
    if line == "quit" then
        return ()
    else do
        res <- runExceptT $ runContT (interpret env line) (lift . putStrLn . display)
        case res of
            Left e -> print e
            Right _ -> return ()
        main' env

main = baseEnv >>= main'
