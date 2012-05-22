
module FFL (
    readRule,
    first1,
    follow1,
    haveEmpty
) where

import Data.Char
import Data.List
import Data.Maybe
import Debug.Trace

data Rule = Rule
   { ruleHead :: Char
   , ruleText :: String
   } deriving Show

readRule (n:':':xs) = Rule n xs
firstNTerm = dropWhile (\x-> x == '&' || isTerm x)
findRules s = (map ruleText) . (filter ((s==) . ruleHead))

isTerm c = isLower c || elem c "()+-*/="
removeEmpty = filter (/='&')


follow1 rules a = sort $ nub $ removeEmpty $ follow a [] '$' -- '$' means nothing
    where 
          follow s acc visited = 
            let newRules = filter (\r -> (ruleHead r) /= visited) rules
                cutted = filter ((/=[]) . ruleText) (map (\r -> 
                    Rule (ruleHead r) (dropWhile (/=s) $ ruleText r)) 
                         newRules)
                needTopFollow = map ruleHead $ filter (\r-> 
                                length (ruleText r) == 1 || 
                                haveEmpty rules (drop 1 (ruleText r))) cutted
                openTopFollow = concatMap (\r -> 
                                        follow r [] s) needTopFollow
                haveSuffics = map ((drop 1) . ruleText) $
                              filter (\r -> length (ruleText r) > 1) cutted
                openSuffics = concatMap (\r -> first1 rules r) haveSuffics
            in
                (if s == 'S' then ['$'] else []) ++ openTopFollow ++ openSuffics
          

first1 rules str = nub $ sort $ first str [] []
    where 
    first [] acc _ = acc
    first (s:ss) acc visited
            | (s:ss) == "&" = '&':acc
            | isTerm  s = s:acc
            | ss == [] = openRules $ findRules s rules
            | not $ haveEmpty rules [s] = first [s] acc (s:visited)
            | True = removeEmpty (first [s] acc (s:visited)) ++
                     (first ss acc (s:visited))
                where 
                    openRules xs = 
                        let maybeNews = findNews xs in
                        concatMap (\x-> first x acc (s:visited)) maybeNews 
                    findNews xs = fmap (
                                    \(a:ab)-> if not $ elem a visited
                                              then a:ab
                                              else if haveEmpty rules [a]
                                                   then ab
                                                   else []

                                    ) xs

haveEmpty rules xs = haveEmpty' rules xs []
haveEmpty' rules (s:ss) visited 
    | (s:ss) == "&" = True
    | isTerm s      = False
    | ss == []      = hv $ filter ( (==Nothing) . find isTerm) (findRules s rules)
    | True = (haveEmpty' rules [s] visited) && (haveEmpty' rules ss (s:visited))
        where hv xs = 
                let newNs = filter ( \(x:_)-> not $ elem x visited ) xs in
                any (==True) $ map (\x->haveEmpty' rules x (s:visited)) newNs















    

        

