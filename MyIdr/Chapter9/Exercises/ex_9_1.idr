-- Exercise 1
data Elem : a -> List a -> Type where
    Here : Elem x (x :: xs)
    There : (later : Elem x xs) -> Elem x (y :: xs)

-- Exercise 2
data Last : List a -> a -> Type where
    LastOne : Last [value] value
    LastCons : (later: Last xs value) -> Last (x :: xs) value

last123 : Last [1,2,3] 3
last123 = LastCons (LastCons LastOne)

nilNotLast : Last [] value -> Void
nilNotLast LastOne impossible
nilNotLast (LastCons _) impossible

consNotLast : (notLast : Last (y :: xs) value -> Void) -> Last (x :: (y :: xs)) value -> Void
consNotLast notLast (LastCons later) = notLast later 

lastNotEqual : (contra : (x = value) -> Void) -> Last [x] value -> Void
lastNotEqual contra LastOne = contra Refl
lastNotEqual contra (LastCons later) = nilNotLast later

isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No nilNotLast
isLast (x :: []) value = (case decEq x value of
                                   (Yes Refl) => Yes LastOne
                                   (No contra) => No (lastNotEqual contra))
isLast (x :: (y :: xs)) value = case isLast (y :: xs) value of
                              (Yes prf) => Yes (LastCons prf)
                              (No notLast) => No (consNotLast notLast)
