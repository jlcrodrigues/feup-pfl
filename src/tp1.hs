module Main where
import Data.List ( groupBy, sort, sortBy )
import Data.Char (toLower, digitToInt, isDigit, isAlphaNum)

type Mon = (Int, [(String, Int)]) -- (coefficient, variables)
type Pol = [Mon]


------------------------ input ------------------------

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn d s = if xs == ""
    then x : (splitOn d xs)
    else x : (splitOn d (tail xs))
    where (x, xs) = break (==d) s

-- Read a number from a string until a non digit char is found
readNumber :: String -> (Int, String)
readNumber m =  until 
    (\x -> if snd x /= "" then not (isDigit (head (snd x))) else True) 
    (\x -> (fst x * 10 + digitToInt (head (snd x)), tail (snd x))) 
    (0, m)

-- Read chars from a string until a non alphabetic char is found
readVariable :: String -> (String, String)
readVariable s = until 
    (\x -> if not (snd x == "") then head (snd x) == '^' else True) 
    (\x -> (fst x ++ [head (snd x)], tail (snd x))) 
    ("", s)

-- Read a variable and exponent list from a string
readVariables :: String -> [(String, Int)]
readVariables "" = [("", 0)]
readVariables s = 
    if (exp_str /= "" && v /= "") 
        then [(v, exp)] ++ readVariables (rest)
        else [(v, 1)]
    where (v, exp_str) = readVariable s 
          (exp, rest) = readNumber (tail exp_str)

-- Parse monomial
toMon :: String -> Mon
toMon "" = toMon "0"
toMon m = 
    if (c /= 0)
        then (c, v)
        else if not (isDigit (head m))
            then (1, v)
        else (0, v) 
    where (c, v1) = readNumber m
          v = readVariables v1

-- Parse polynomial
toPol :: String -> Pol
toPol s = map toMon (splitOn '+' (filter (\x -> (isAlphaNum x) || x == '^' || x == '+') s))


------------------------ operations ------------------------

getCoefficient :: Mon -> Int
getCoefficient (c, _) = c

getVariables :: Mon -> [(String, Int)]
getVariables (_, v) = v

-- Collapse same variable powers
simplifyMon :: Mon -> Mon
simplifyMon (c, v) = (c, [(fst (x!!0), sum [e | (_, e)<-x]) | x <- same])
    where same = (groupBy (\(v1, _) (v2, _) -> (v1 == v2)) (sort v))

-- Determine if a monomial should appear first in normal order
compareMon :: Mon -> Mon -> Ordering
compareMon (_, v1) (_, v2) = if ((sort v1)!!0 >= (sort v2)!!0) then GT else LT

-- Sort a polynomial according to its variable and exponent
sortPol :: Pol -> Pol
sortPol p = sortBy (\x y -> compareMon x y) p

-- Group monomials with same variable and degree
likeTerms :: Pol -> [Pol]
likeTerms p = groupBy (\(_, v1) (_, v2) -> (sort v1 == sort v2)) (sortPol p)

-- Normalize polynomials
polNormalize :: Pol -> Pol
polNormalize p = filter (\x -> fst x /= 0) [(sum [c | (c, _)<-x], getVariables (x!!0)) | x <- likeTerms p]

-- Add polynomials
polAdd :: Pol -> Pol -> Pol
polAdd a b = polNormalize (a ++ b)

-- Multiply two monomials
monMultiply :: Mon -> Mon -> Mon
monMultiply (c1, vs1) (c2, vs2) = simplifyMon (c1 * c2, vs1 ++ vs2)

-- Multiply polynomials
polMultiply :: Pol -> Pol -> Pol
polMultiply a b = [monMultiply x y | x<-a, y<-b]

-- Derivate polynomials
polDerivate :: Pol -> Pol
polDerivate p = [(c * sum [e | (v, e)<-vs], [(v, e - 1)| (v, e)<-vs]) | (c, vs)<-polNormalize p]

------------------------ output ------------------------

-- Output polynomial
printPol :: Pol -> IO()
printPol [] = putStr "\n"
printPol (x:xs) = 
    do
        if fst x /= 0 && fst x /= 1
            then putStr (show (abs (fst x)))
        else putStr ""

        if fst x /= 0
            then parseVariables (snd x)
        else putStr ""

        if xs == [] || fst x == 0
            then putStr ""
            else 
                if fst (head xs) > 0
                    then putStr " + "
                else if fst (head xs) == 0
                    then putStr ""
                else putStr " - "

        printPol xs
        
-- Normalize a polynomial from a String
normalize :: String -> IO()
normalize s = do printPol (polNormalize (toPol s))

-- Add polynomials from strings
add :: String -> String -> IO()
add a b = do printPol (polAdd (toPol a) (toPol b))

-- Multiply polynomials from strings
multiply :: String -> String -> IO()
multiply a b = printPol (polMultiply (toPol a) (toPol b))

-- Derivate a polynomial from a string
derivate :: String -> IO()
derivate p = printPol (polDerivate (toPol p))



parseVariables :: [(String, Int)] -> IO ()
parseVariables [] = putStr ""
parseVariables (x:xs) = 
    do 
        if exp > 0
            then do 
                if (fst x /= "") then do
                    putStr (fst x)
                    if exp > 1
                        then do 
                            putStr "^"  
                            putStr (show exp)
                    else putStr ""
                else putStr ""

        else if exp < 0
            then do 
                if (fst x /= "") then do
                    putStr (fst x)
                    putStr "^"   
                    putStr "("; putStr (show exp); putStr ")"
                    else putStr ""

        else putStr ""

        if xs == []
            then putStr ""
            else if (fst (head xs) /= "" && snd (head xs) > 0 && exp > 0)
                then putChar '*'
            else putStr ""

        parseVariables xs
        where exp = snd x



showMenu :: IO ()
showMenu = do 
    putStrLn "\nChoose an option:"
    putStrLn "a) normalize a polynomial"
    putStrLn "b) add polynomials"
    putStrLn "c) multiply polynomials"
    putStrLn "d) derive a polynomial"
    putStrLn "e) exit"
    putStr "Choice:"


solveA :: IO ()
solveA = do
    putStr "Non normalized polynomial: "
    polStr <- getLine
    let pol = polNormalize (toPol polStr)
    putStrLn ("Normalized polynomial: ")
    printPol pol

solveB :: IO ()
solveB = do
    putStr "First polynomial: "
    polStr <- getLine
    let pol1 = toPol polStr
    putStr "Second polynomial: "
    polStr <- getLine
    let pol2 = toPol polStr
    putStrLn ("Pol 1 + Pol 2 = ") 
    printPol (polAdd pol1 pol2)

solveC :: IO ()
solveC = do
    putStr "First polynomial: "
    polStr <- getLine
    let pol1 = toPol polStr
    putStr "Second polynomial: "
    polStr <- getLine
    let pol2 = toPol polStr
    putStrLn ("Pol 1 * Pol 2 = ")
    printPol (polMultiply pol1 pol2)

solveD :: IO ()
solveD = do
    putStr "Derivable polynomial: "
    polStr <- getLine
    let pol = toPol polStr
    putStrLn ("Derived polynomial: ")
    printPol (polDerivate pol)



cycler :: IO ()
cycler  =
    do 
        showMenu
        choice <- getLine
        putChar '\n'
        case map toLower choice of
            "a" -> do solveA; cycler
            "b" -> do solveB; cycler
            "c" -> do solveC; cycler
            "d" -> do solveD; cycler
            "e" -> putStr ""
            _ -> do putStr "Invalid option!\n"; cycler


main :: IO ()
main = cycler

