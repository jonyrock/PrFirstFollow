import System.Environment
import FFL
import Utils

main = do
    args <- getArgs
    if args == [] then error "Need input file with grammar" else return ()
    let a = head args
    rules <- readRulesFromFile a
    return ()
