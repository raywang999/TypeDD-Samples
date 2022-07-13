data Tree elem = Empty
    | Node (Tree elem) elem (Tree elem)

insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x (Node left y right) = case compare x y of
                                    LT => Node (insert x left) y right
                                    EQ => Node left y right
                                    GT => Node left y (insert x right)

-- Exercise 1
listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

-- Exercise 2
treeToList : Tree a -> List a
treeToList Empty = []
treeToList (Node left val right) = ((treeToList left) ++ [val]) ++ (treeToList right)


-- Exercise 3
data Expr : Type where
    Val : Int -> Expr
    Add : Expr -> Expr -> Expr
    Sub : Expr -> Expr -> Expr 
    Mult : Expr -> Expr -> Expr

-- Exercise 4
evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add x y) = evaluate x + evaluate y
evaluate (Sub x y) = evaluate y - evaluate x
evaluate (Mult x y) = evaluate x * evaluate y

-- Exercise 5
maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe x Nothing = Nothing
maxMaybe Nothing y = Nothing
maxMaybe (Just x) (Just y) = Just (max x y)

data Shape : Type where
    ||| A Triangle with base and height
    Triangle : Double -> Double -> Shape
    Rectangle : Double -> Double -> Shape
    Circle : Double -> Shape

area : Shape -> Double
area (Circle r) = pi * r * r
area (Triangle base height) = base * height * 0.5
area (Rectangle length width) = length * width

data Picture : Type where
    Primitive : Shape -> Picture
    Combine : Picture -> Picture -> Picture
    Rotate : Double -> Picture -> Picture
    Translate : Double -> Double -> Picture -> Picture

-- Exercise 6
%name Shape shape, shape1, shape2
%name Picture pic, pic1, pic2 
biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive shape) = case shape of
                                         (Triangle x y) => Just (0.5 * x * y)
                                         nonTriangle => Nothing
biggestTriangle (Combine pic pic1) = maxMaybe (biggestTriangle pic) (biggestTriangle pic1)
biggestTriangle (Rotate x pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic

testPic1 : Picture
testPic1 = Combine (Primitive (Triangle 2 3)) (Primitive (Triangle 2 4))
testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 1 3)) (Primitive (Circle 4))
