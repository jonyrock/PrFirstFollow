-- проверить выводимость правил
isWayFromStart rules s t = True                    
follow1 rules s = map (\x-> if x == '&' then '$' else x) $ nub $ sort $ follow
    where follow = concat $ map (findPostfix . ruleText) 
                           (filter (isWayFromStart rules 'S' . ruleHead) rules)
          findPostfix r | elem s r = emptyFix $ first1 rules 
                                                (drop 1 $ dropWhile (/=s) r)
                        | True = []
          emptyFix [] = ['$']
          emptyFix xs = xs
          
--follow1 rules s = let type1 = if findRules s /= [] then ['$'] else []
--                      type2 =  findRules s
--                  in un



g2 rules = concat $ take (length rules) (repeat rules)
follow1 rules s = (findAllFollows (rules) [('S', "$")])
    where findAllFollows [] acc = acc
          findAllFollows (x:xs) acc = findAllFollows xs 
                (forEachRule (ruleHead x) (firstNTerm $ ruleText x) acc)
          forEachRule _ [] acc = acc
          forEachRule a (nb:b) acc | b == [] || haveEmpty rules b 
                                     = 
                                            addToCash (nb, follow1 rules a) acc
                                     
                                   | True = 
                                        traceShow a $ 
                                        traceShow b $
                                        traceShow (first1 rules b) $
                                        addToCash (nb, (
                                                removeEmpty $ first1 rules b)) acc
