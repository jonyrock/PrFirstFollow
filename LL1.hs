import System.Environment
import Control.Monad
import FFL
import Utils

main = do
    args <- getArgs
    when (null args) $ error "Need input file with grammar"
    let a = head args
    rules <- readRulesFromFile a
    putStrLn $ show $ isLL1 rules
