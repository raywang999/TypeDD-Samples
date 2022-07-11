import Data.Vect

describeList : List Int -> String
describeList [] = "empty"
describeList (x::xs) = "non empty" ++ show xs

xor : Bool -> Bool -> Bool
xor False y = y
xor True y = not y

isEven : Nat -> Bool
isEven Z = True
isEven (S k) = not (isEven k)

addMatrix : Num numType => 
    Vect rows (Vect cols numType) -> 
    Vect rows (Vect cols numType) -> 
    Vect rows (Vect cols numType) 

multMatrix : Num numType => 
    Vect n (Vect m numType) -> 
    Vect m (Vect p numType) -> 
    Vect n (Vect p numType) 

createEmpty : Vect m (Vect 0 a)
createEmpty = replicate _ []

my_transposeMatrix : Vect n (Vect m a) -> Vect m (Vect n a)
my_transposeMatrix [] = createEmpty
my_transposeMatrix (x :: xs) = let transposedMatrix = my_transposeMatrix xs in 
    transposeHelper x transposedMatrix where
        transposeHelper : (x : Vect m a) -> (transposedMatrix : Vect m (Vect len a)) -> Vect m (Vect (S len) a)
        transposeHelper [] [] = []
        transposeHelper (x :: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys

createEmpties : Vect n (Vect 0 a)
createEmpties {n = Z} = []
createEmpties {n = (S k)} = [] :: createEmpties
