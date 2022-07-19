data Expr num = Val num 
          | Add (Expr num) (Expr num)
          | Sub (Expr num) (Expr num)
          | Mul (Expr num) (Expr num)
          | Div (Expr num) (Expr num)
          | Abs (Expr num)

eval : (Abs num, Neg num, Integral num) => Expr num -> num          
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Abs x) = abs (eval x)

Num ty => Num (Expr ty) where
  (+) = Add
  (*) = Mul
  fromInteger = Val . fromInteger

  -- Exercise 1
Functor Expr where
  map func (Val x) = Val (func x)
  map func (Add x y) = Add (map func x) (map func y)
  map func (Sub x y) = Sub (map func x) (map func y)
  map func (Mul x y) = Mul (map func x) (map func y)
  map func (Div x y) = Div (map func x) (map func y)
  map func (Abs x) = Abs (map func x)

  -- Exercise 2
data Vect : (size : Nat) -> Type -> Type where
    Nil : Vect Z a
    (::) : (x : a) -> (xs : Vect len a) -> Vect (S len) a

Eq a => Eq (Vect n a) where
  (==) [] [] = True
  (==) (x :: xs) (y :: ys) = x == y && xs == ys

Foldable (Vect n) where
  foldr func init [] = init
  foldr func init (x :: xs) = func x (foldr func init xs)
  foldl func init [] = init
  foldl func init (x :: xs) = foldl func (func init x) xs

