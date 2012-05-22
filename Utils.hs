module Utils (
    readRulesFromFile
) where

import FFL

readRulesFromFile = fmap (map readRule . 
                    takeWhile (/= "---------") . lines) . readFile

