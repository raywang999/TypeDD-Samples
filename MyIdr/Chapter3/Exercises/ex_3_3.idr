import Data.Vect

createEmpty : Vect m (Vect 0 a)
createEmpty = replicate _ []

my_transposeMatrix : Vect n (Vect m a) -> Vect m (Vect n a)
my_transposeMatrix [] = createEmpty
my_transposeMatrix (x :: xs) = let transposedMatrix = my_transposeMatrix xs in 
    zipWith f x transposedMatrix where 
        f : a -> (transposedMatrix : Vect len a) -> Vect (S len) a
        f x transposedMatrix = x :: transposedMatrix

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix xs ys = zipWith sumRow xs ys where
    sumRow : Num a => (xs : Vect m a) -> (ys : Vect m a) -> Vect m a 
    sumRow xs ys = zipWith (+) xs ys

process : Num numType => (x : Vect m numType) -> (transys : Vect p (Vect m numType)) -> Vect p numType
process x [] = []
process x (transy :: transys) = (sum (zipWith (*) x transy)) :: (process x transys)

multMatrix : Num numType => 
    Vect n (Vect m numType) -> 
    Vect m (Vect p numType) -> 
    Vect n (Vect p numType) 
multMatrix [] ys = []
multMatrix (x :: xs) ys = process x (my_transposeMatrix ys) :: multMatrix xs ys
