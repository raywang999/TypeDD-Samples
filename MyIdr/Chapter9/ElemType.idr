data Vect : (len : Nat) -> (elem : Type) -> Type where
    Nil : Vect 0 elem
    (::) : elem -> Vect n elem -> Vect (S n ) elem

data Elem : a -> Vect n a -> Type where
    Here : Elem x (x :: xs)
    There : (later : Elem x xs) -> Elem x (y :: xs)

notInNil : Elem value [] -> Void
notInNil Here impossible
notInNil (There _) impossible

notInTail : (notHere : (x = value) -> Void) -> (notThere : Elem value y -> Void) -> Elem value (x :: y) -> Void
notInTail notHere notThere Here = notHere Refl
notInTail notHere notThere (There later) = notThere later

isElem : DecEq a => (value : a) -> (xs : Vect n a) -> Dec (Elem value xs)
isElem value [] = No notInNil
isElem value (x :: y) = case decEq x value of
                             (Yes Refl) => Yes Here
                             (No notHere) => (case isElem value y of
                                                  (Yes prf) => Yes (There prf)
                                                  (No notThere) => No (notInTail notHere notThere))
