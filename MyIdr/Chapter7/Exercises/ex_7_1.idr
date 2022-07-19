data Shape = Triangle Double Double
    | Rectangle Double Double
    | Circle Double


Eq Shape where
  (==) (Triangle b h) (Triangle b' h')
        = b == b' && h == h'
  (==) (Rectangle l w) (Rectangle l' w') 
        = l == l' && w == w'
  (==) (Circle r) (Circle r') = r == r'
  (==) _ _ = False

Ord Shape where
  compare x y = compare (area x) (area y) where 
    area : Shape -> Double
    area (Triangle b h) = 0.5 * b * h
    area (Rectangle l w) = l * w
    area (Circle r) = pi * r * r

testShapes : List Shape
testShapes = [Circle 3, Triangle 3 9, Rectangle 2 6, Circle 4,
Rectangle 2 7]