module Main where
import Data.List ( groupBy, sort, sortBy )
import Data.Char (toLower)

type Mon = (Int, [(Char, Int)]) -- (coefficient, variables)
type Pol = [Mon]

getCoefficient :: Mon -> Int
getCoefficient (c, _) = c

getVariables :: Mon -> [(Char, Int)]
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
polNormalize p = [(sum [c | (c, _)<-x], getVariables (x!!0)) | x <- likeTerms p]

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

-- Parse polynomial
toPol :: IO String -> Pol
toPol s = []

-- Output polynomial
printPol :: Pol -> IO()
-- type Mon = (Int, [(Char, Int)]) -- (coefficient, variables)
-- type Pol = [Mon]
printPol [] = putStr "\n"
printPol (x:xs) = 
    do
        if fst x > 0
            then putStr " + "
        else if fst x == 0
            then putStr ""
        else putStr " - "

        if fst x /= 1 && fst x /= 0
            then putStr (show (abs (fst x)))
        else putStr ""

        if fst x /= 0
            then parseVariables (snd x)
        else putStr ""

        printPol xs
        


parseVariables :: [(Char, Int)] -> IO ()
parseVariables [] = putStr ""
parseVariables (x:xs) = 
    do 
        if exp > 0
            then do 
                putChar (fst x)
                if exp > 1
                    then do 
                        putStr "^"  
                        putStr (show exp)
                else putStr ""

        else if exp < 0
            then do 
                putChar (fst x)
                putStr "^"   
                putStr "("; putStr (show exp); putStr ")"

        else putStr ""

        if null xs || exp == 0
            then putStr ""
        else putStr "*"

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
    -- parse pol from String to Pol
    -- let polParsed = polNormalize polParsed
    putStrLn ("Normalized polynomial: ") -- ++ polParsed

solveB :: IO ()
solveB = do
    putStr "First polynomial: "
    polStr1 <- getLine
    -- parse pol from String to Pol
    putStr "Second polynomial: "
    polStr2 <- getLine
    -- parse pol from String to Pol
    putStrLn ("Pol 1 + Pol 2 = ") -- ++ polAdd polParsed1 polParsed2

solveC :: IO ()
solveC = do
    putStr "First polynomial: "
    polStr1 <- getLine
    -- parse pol from String to Pol
    putStr "Second polynomial: "
    polStr2 <- getLine
    -- parse pol from String to Pol
    putStrLn ("Pol 1 * Pol 2 = " ++ polStr1) -- ++ polMultiply polParsed1 polParsed2

solveD :: IO ()
solveD = do
    putStr "Derivable polynomial: "
    polStr <- getLine
    -- parse pol from String to Pol
    -- let polParsed = polDerivate polParsed
    putStrLn ("Derived polynomial: ") -- ++ polParsed



cicle :: IO ()
cicle  =
    do 
        showMenu
        choice <- getLine
        putChar '\n'
        case map toLower choice of
            "a" -> do solveA; cicle
            "b" -> do solveB; cicle
            "c" -> do solveC; cicle
            "d" -> do solveD; cicle
            "e" -> putStr ""


main :: IO ()
main = putStrLn "Apenas teste"

