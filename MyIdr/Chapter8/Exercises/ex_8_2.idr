import Data.Vect

myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m = sym (plusZeroRightNeutral m)
myPlusCommutes (S k) m = rewrite myPlusCommutes k m in 
    plusSuccRightSucc m k

myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs where
    reverse' : (acc : Vect n a) -> (xs : Vect m a) -> Vect (n + m) a
    reverse' acc [] = reverseProof_nil acc where
        reverseProof_nil : (acc : Vect n1 a) -> Vect (plus n1 0) a
        reverseProof_nil {n1} acc = rewrite plusZeroRightNeutral n1 in acc
    reverse' acc (x :: xs) = reverseProof_xs (reverse' (x :: acc) xs) where
        reverseProof_xs : Vect ((S n) + len) a -> Vect (plus n (S len)) a
        reverseProof_xs {n} {len} xs = rewrite sym (plusSuccRightSucc n len) in xs


