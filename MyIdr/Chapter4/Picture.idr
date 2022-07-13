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

testPicture : Picture
testPicture = Combine (Translate 5 5 (Primitive (Rectangle 20 10))) 
    (Combine (Translate 15 25 (Primitive (Triangle 10 10))) (Translate 35 5 (Primitive (Circle 5))))

%name Shape shape, shape1, shape2
%name Picture pic, pic1, pic2 
pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic pic1) = pictureArea pic + pictureArea pic1
pictureArea (Rotate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic

