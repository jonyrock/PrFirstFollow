module Utils (
    readRulesFromFile
) where

import FFL

readRulesFromFile fileName = do  
    content <- readFile fileName
    let cLines = lines content
    let rules = map readRule $ takeWhile (/="---------") cLines
    return rules

