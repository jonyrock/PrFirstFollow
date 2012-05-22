import System.IO
import FFL

readRulesFromFile fileName = do  
    content <- readFile fileName
    let cLines = lines content
    let rules = map readRule $ takeWhile (/="---------") cLines
    return rules

rulesTest1 = readRulesFromFile "gr1.txt"
rulesTest2 = readRulesFromFile "gr2.txt"
rulesTest3 = readRulesFromFile "gr3.txt"
rulesTest4 = readRulesFromFile "gr4.txt"
rulesTest5 = readRulesFromFile "gr5.txt"

testOK True = "OK"
testOK False = "FAIL"

testHaveEmpty = do
    rules1 <- rulesTest1
    rules2 <- rulesTest2
    putStrLn $ "t1: S -> e (False): " 
        ++ (testOK $ (haveEmpty rules1 "S") == False)
    putStrLn $ "t1: C -> e (True): " 
        ++ (testOK $ (haveEmpty rules1 "C") == True)
    putStrLn $ "t2: S -> e (False): " 
        ++ (testOK $ (haveEmpty rules2 "S") == False)
    putStrLn $ "t2: AC -> e (True): " 
        ++ (testOK $ (haveEmpty rules2 "AC") == True)
    putStrLn $ "t2: ACCAA -> e (True): " 
        ++ (testOK $ (haveEmpty rules2 "ACCAA") == True)
    putStrLn $ "t2: ABAC -> e (False): " 
        ++ (testOK $ (haveEmpty rules2 "ABAC") == False)
    putStrLn $ "t2: AAA -> e (True): " 
        ++ (testOK $ (haveEmpty rules2 "AAA") == True)        
  

testFirst1 = do
        rules <- rulesTest1
        rules2 <- rulesTest2
        putStrLn $ "t1: First1(ab) = {a}: " ++ 
         (testOK $ first1 rules "ab" == ['a'])
        putStrLn $ "t1: First1(&) = {&}: " ++ 
            (testOK $ first1 rules "&" == ['&'])
        putStrLn $ "t1: First1(bAC) = {b}: " ++ 
            (testOK $ first1 rules "bAc" == ['b'])
        putStrLn $ "t1: First1(C) = {c, &}: " ++ 
         (testOK $ first1 rules "C" == ['&','c'])
        putStrLn $ "t1: First1(CA) = {a, b, c}: " ++ 
            (testOK $ first1 rules "CA" == ['a', 'b', 'c'])
        putStrLn $ "t2: First1(S) = {a, b, c, e}: " ++ 
            (testOK $ first1 rules2 "S" == ['a', 'b', 'c', 'e'])
        rules3 <- rulesTest3
        putStrLn $ "t3: First(E) = {(, i}: " 
            ++ (testOK $ (first1 rules3 "E") == ['(', 'i'])
        putStrLn $ "t3: First(T) = {(, i}: " 
            ++ (testOK $ (first1 rules3 "T") == ['(', 'i'])
        putStrLn $ "t3: First(S) = {(, i}: " 
            ++ (testOK $ (first1 rules3 "S") == ['(', 'i'])
        putStrLn $ "t3: First(R) = {&, +}: " 
            ++ (testOK $ (first1 rules3 "R") == ['&', '+'])
        putStrLn $ "t3: First(Y) = {&, *}: " 
            ++ (testOK $ (first1 rules3 "Y") == ['&', '*'])

testFollow1 = do
        rules1 <- rulesTest1
        rules2 <- rulesTest2
        rules4 <- rulesTest4
        rules5 <- rulesTest5
        putStrLn $ "t1: Follow(A) = {$, c}: " 
            ++ (testOK $ (follow1 rules1 'A') == ['$', 'c'])
        putStrLn $ "t1: Follow(B) = {$, c}: " 
            ++ (testOK $ (follow1 rules1 'B') == ['$', 'c'])
        putStrLn $ "t1: Follow(C) = {$, c}: " 
            ++ (testOK $ (follow1 rules1 'C') == ['$', 'c'])
        putStrLn $ "t1: Follow(S) = {$}: " 
            ++ (testOK $ (follow1 rules1 'S') == ['$'])
        putStrLn $ "t2: Follow(E) = {$, e}: " 
            ++ (testOK $ (follow1 rules2 'E') == ['$'])
        putStrLn $ "t2: Follow(A) = {$, c, e}: " 
            ++ (testOK $ (follow1 rules2 'A') == ['c', 'e'])
        putStrLn $ "t4: Follow(S) = {$, )}: " 
            ++ (testOK $ (follow1 rules4 'S') == ['$', ')'])
        putStrLn $ "t4: Follow(R) = {$, )}: " 
            ++ (testOK $ (follow1 rules4 'R') == ['$', ')'])
        putStrLn $ "t4: Follow(T) = {$, ), +}: " 
            ++ (testOK $ (follow1 rules4 'T') == ['$', ')', '+'])            
        putStrLn $ "t4: Follow(Y) = {$, ), +}: " 
            ++ (testOK $ (follow1 rules4 'Y') == ['$', ')', '+'])
        putStrLn $ "t4: Follow(F) = {$, ), *, +}: " 
            ++ (testOK $ (follow1 rules4 'F') == ['$', ')', '*', '+'])         
--        putStrLn $ (follow1 rules5 'A')
        putStrLn $ "t5: Follow(A) = {$}: " 
            ++ (testOK $ (follow1 rules5 'A') == ['$'])
            
            
            
            
            
            

            
