||| Represents Shapes
data Shape = ||| A Triangle, with a base and height
            Triangle Double Double
            | ||| A Circle with a radius
            Circle Double
            | ||| A Rectangle, with a length and width
            Rectangle Double Double

area : Shape -> Double
area (Circle r) = pi * r * r
area (Triangle base height) = base * height * 0.5
area (Rectangle length width) = length * width
