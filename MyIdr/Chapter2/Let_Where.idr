longer : String -> String -> Nat
longer str1 str2 = let
    len1 = length str1
    len2 = length str2 in 
        if len1 > len2 then len1 else len2

pythagoras : Double -> Double -> Double
pythagoras a b = sqrt (square a + square b ) where
    square : Double -> Double
    square x = x * x