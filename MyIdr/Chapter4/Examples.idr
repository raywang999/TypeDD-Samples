data Direction = North | East | South | West

turnClockwise : Direction -> Direction
turnClockwise North = East
turnClockwise East = South
turnClockwise South = West
turnClockwise West = North

||| Represents Shapes
data Shape = ||| A Triangle, with a base and height
            Triangle Double Double
            | ||| A Circle with a radius
            Circle Double
            | ||| A Rectangle, with a length and width
            Rectangle Double Double


data Picture : Type where
    Primitive : Shape -> Picture
    Combine : Picture -> Picture -> Picture
    Rotate : Double -> Picture -> Picture
    Translate : Double -> Double -> Picture -> Picture

testPicture : Picture
testPicture = Combine (Translate 5 5 (Primitive (Rectangle 20 10))) 
    (Combine (Translate 15 25 (Primitive (Triangle 10 10))) (Translate 35 5 (Primitive (Circle 5))))
data Biggest = NoTriangle | Size Double

%name Shape shape, shape1, shape2
%name Picture pic, pic1, pic2
biggestTriangle : Picture -> Biggest
biggestTriangle (Primitive shape) = case shape of
                                         (Triangle base height) => Size (0.5 * base * height)
                                         other => NoTriangle
biggestTriangle (Combine pic pic1) = let 
    btpic = biggestTriangle pic
    btpic1 = biggestTriangle pic1 in
        case (btpic, btpic1) of
              (NoTriangle, btpic1) => btpic1
              (btpic, NoTriangle) => btpic
              (Size btpicsize, Size btpic1size) => Size (max btpicsize btpic1size)
biggestTriangle (Rotate x pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic

data My_Maybe valtype =
      Nothing
    | Just valtype

safeDivide : Double -> Double -> My_Maybe Double
safeDivide x y = if y == 0 then Nothing else Just (x / y)

    
