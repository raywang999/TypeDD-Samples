module ex_10_3

import DataStore

getValues : DataStore schema -> List (SchemaType schema)
getValues x with (storeView x)
  getValues x | SNil = []
  getValues (addToStore value store) | (SAdd rec) 
    = value :: getValues store | rec

testStore : DataStore (SString .+. SInt)
testStore = addToStore ("First",1) $
            addToStore ("Second",2) $
            empty

-- Exercise 2            
export
data Shape = Triangle Double Double
    | Rectangle Double Double
    | Circle Double

export
triangle : Double -> Double -> Shape
triangle = Triangle

export
rectangle : Double -> Double -> Shape
rectangle = Rectangle

export
circle : Double -> Shape
circle = Circle

public export
data ShapeView : Shape -> Type where
  STriangle : (base : Double) -> (height : Double) -> ShapeView (triangle base height)
  SRectangle : (length : Double) -> (height : Double) -> ShapeView (rectangle length height)
  SCircle : (radius : Double) -> ShapeView (circle radius)

export 
shapeView : (shape: Shape) -> ShapeView shape
shapeView (Triangle x y) = STriangle x y
shapeView (Rectangle x y) = SRectangle x y
shapeView (Circle x) = SCircle x

