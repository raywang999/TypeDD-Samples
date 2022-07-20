import Data.Vect

append_nil : (ys : Vect m a) -> Vect (plus m 0) a
append_nil {m} ys = rewrite plusZeroRightNeutral m in ys

append_xs : Vect (S (m + len)) a -> Vect (plus m (S len)) a
append_xs {m} {len} xs = rewrite sym (plusSuccRightSucc m len) in xs

append : Vect n a -> Vect m a -> Vect (m + n) a 
append [] ys = append_nil ys 
append (x :: xs) ys = append_xs (x :: append xs ys)

