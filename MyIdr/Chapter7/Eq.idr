data Matter = Liquid | Solid | Gas

Eq Matter where
  (==) Liquid Liquid = True
  (==) Solid Solid = True
  (==) Gas Solid = True
  (==) _ _ = False
  (/=) x y = not (x == y)

occurrences : Eq ty => (item : ty) -> (values : List ty) -> Nat
occurrences item [] = Z
occurrences item (value :: values) = case item == value of
                                  False => occurrences item values
                                  True => S (occurrences item values)

data Tree elem = Empty
    | Node (Tree elem) elem (Tree elem)

Eq elem => Eq (Tree elem) where
  (==) Empty Empty = True
  (==) (Node left e right) (Node left' e' right') 
        = left == left' && e == e' && right == right'
  (==) _ _ = False
