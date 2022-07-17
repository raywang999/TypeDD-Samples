import Data.Vect

Position : Type
Position = (Double, Double)

Polygon : Nat -> Type
Polygon n = Vect n Position

tri_v1 : Vect 3 (Double, Double) 
tri_v1 = [(0,0),(3,0),(0,4)]

tri_v2 : Vect 3 Position
tri_v2 = [(0,0),(3,0),(0,4)]

tri : Polygon 3
tri = [(0,0),(3,0),(0,4)]
