import Data.Vect

myReverse_v1 : Vect n a -> Vect n a 
myReverse_v1 [] = []
myReverse_v1 {n = S k} (x :: xs) = let result = myReverse_v1 xs ++ [x] in
    rewrite plusCommutative 1 k in result

myReverse : Vect n a -> Vect n a 
myReverse [] = []
myReverse {n = S k} (x :: xs) = reverseProof (myReverse xs ++ [x])
    where 
        reverseProof : Vect (k + 1) a -> Vect (S k) a
        reverseProof result = rewrite plusCommutative 1 k in result

    
