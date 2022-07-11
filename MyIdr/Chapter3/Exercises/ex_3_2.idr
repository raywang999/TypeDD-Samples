import Data.Vect

my_length : List a -> Nat
my_length [] = 0
my_length (a :: as) = S (my_length as)

my_reverse : List a -> List a
my_reverse [] = []
my_reverse (x :: xs) = let reversedXs = my_reverse xs in
    insert x reversedXs where
        insert : (x : a) -> (reversedXs : List a) -> List a
        insert x [] = [x]
        insert x (y :: xs) = y :: (insert x xs)

my_map : (a -> b) -> List a -> List b
my_map f [] = []
my_map f (a :: as) = f a :: my_map f as

my_vect_map : (a -> b) -> Vect n a -> Vect n b
my_vect_map f [] = []
my_vect_map f (a :: as) = f a :: my_vect_map f as

