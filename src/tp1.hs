import Data.List

type Mon = (Int, Char, Int) -- (coefficient, variable, exponent)
type Pol = [Mon]

-- 2*y^2 + 5*z = [(2,y,2), (5,z,1)] 

getCoefficient :: Mon -> Int
getCoefficient (c, _, _) = c

getVariable :: Mon -> Char
getVariable (_, v, _) = v

getExponent :: Mon -> Int
getExponent (_, _, e) = e

-- Group monomials with same variable and degree
likeTerms :: Pol -> [Pol]
likeTerms p = groupBy (\(_, v1, e1) (_, v2, e2) -> v1 == v2 && e1 == e2) p

-- Normalize polynomials
polNormalize :: Pol -> Pol
polNormalize p = [(sum [c | (c, _, _)<-x], getVariable (x!!0), getExponent (x!!0)) | x <- likeTerms p]

-- Add polynomials
polAdd :: Pol -> Pol -> Pol
polAdd a b = a

-- Multiply polynomials
polMultiply :: Pol -> Pol -> Pol
polMultiply a b = a

-- Derivate polynomials
polDerivate :: Pol -> Pol
polDerivate p = p

-- Parse polynomial
toPol :: String -> Pol
toPol s = []

-- Output polynomial
toString :: Pol -> String
toString p = ""