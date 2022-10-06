type Mon = (Int, String, Int)
type Pol = [Mon]

-- Normalize polynomials
polNormalize :: Pol -> Pol

-- Add polynomials
polAdd :: Pol -> Pol -> Pol

-- Multiply polynomials
polMultiply :: Pol -> Pol -> Pol

-- Derivate polynomials
polDerivate :: Pol -> Pol

-- Parse polynomial
toPol :: String -> Pol

-- Output polynomial
toString :: Pol -> String