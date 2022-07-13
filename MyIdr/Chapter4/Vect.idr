data Vect : (size : Nat) -> Type -> Type where
    Nil : Vect Z a
    (::) : (x : a) -> (xs : Vect len a) -> Vect (S len) a

%name Vect xs, ys, zs

append : Vect n a -> Vect m a -> Vect (n + m) a
append [] ys = ys
append (x :: xs) ys = x :: append xs ys

zip : Vect n a -> Vect n b -> Vect n (a, b)
zip [] [] = []
zip (x :: xs) (y :: ys) = (x, y) :: zip xs ys

index : Fin n -> Vect n a -> a
