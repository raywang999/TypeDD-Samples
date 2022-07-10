identityInt : Int -> Int 
identityInt x = x

identityString : String -> String
identityString x = x

identityBool : Bool -> Bool 
identityBool x = x

identity : ty -> ty
identity x = x 

the : (ty: Type) -> ty -> ty
the ty x = x

double : (Num a) => a -> a
double x = x + x

quadruple : (Num a) => a -> a
quadruple x = double (double x)

wordCount : String -> Nat
wordCount str = length (words str)

allLengths : List String -> List Nat
allLengths strs = map length strs
