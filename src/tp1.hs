import Data.List

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
toPol :: String -> Pol
toPol s = []

-- Output polynomial
toString :: Pol -> String
toString p = ""